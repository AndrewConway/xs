/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.frontend

import org.greatcactus.xs.impl.XSFieldInfo
import scala.collection.mutable.ListBuffer
import java.io.OutputStream
import javax.xml.stream.XMLStreamWriter
import org.greatcactus.xs.api.serialization.XMLSerialize
import java.io.ByteArrayOutputStream
import org.greatcactus.xs.api.serialization.XMLDeserialize
import java.util.Locale
import java.io.ByteArrayInputStream
import org.greatcactus.xs.impl.DependencyInjectionCleaningQueue

/**
 * The master access for editing objects. 
 */

class XSEdit(original:AnyRef) {

  val globalDependencyInjections : Set[AnyRef] = Set.empty

  // methods applying to the tree
  
  val uidsForTreeNodes = new UniqueIDSource()
  val dependencyInjectionCleaningQueue = new DependencyInjectionCleaningQueue()
  
  val undoRedo = new UndoRedoManager(original)
  
  val treeRoot : XSTreeNode = XSTreeNode(original,this) // children can then be got via node.getChildren
  //def treeChildren(node:XSTree) : IndexedSeq[XSTree] = node.
  /** The result of all the editing */
  def currentObject = treeRoot.getObject
  // methods applying to changes to the tree
  
  private var treeListeners : Set[XSEditListener]= Set.empty
  private var detailsPanes : List[XSDetailsPane[_]] = Nil
  private var toolbarStatusListeners : Set[ToolbarStatusListener] = Set.empty
  private var activeEditorListeners : Set[ActiveEditorRegistrationListener] = Set.empty
  private var activeEditorCount : Int = 0
  
  def addTreeListener(l:XSEditListener) { synchronized {treeListeners+=l; l.setCurrentlyEditing(Option(currentlyEditing))}}
  def removeTreeListener(l:XSEditListener) { synchronized { treeListeners-=l}}
  def addToolbarStatusListener(l:ToolbarStatusListener) { synchronized {toolbarStatusListeners+=l; l(getToolbarStatus)}}
  def removeToolbarStatusListener(l:ToolbarStatusListener) { synchronized { toolbarStatusListeners-=l}}
  def addDetailsPane(l:XSDetailsPane[_]) { synchronized {detailsPanes::=l; l.setCurrentlyEditing(Option(currentlyEditing))}}
  def removeDetailsPane(l:XSDetailsPane[_]) { synchronized { detailsPanes=detailsPanes.filter{_!=l}}}
  def addActiveEditorListener(l:ActiveEditorRegistrationListener) { synchronized {activeEditorListeners+=l}}
  def removeActiveEditorListener(l:ActiveEditorRegistrationListener) { synchronized { activeEditorListeners-=l}}
  def registerActiveEditor() {
    synchronized {
      activeEditorCount+=1 ; 
      for (l<-activeEditorListeners) l.register() ; 
      if (!dependencyInjectionCleaningQueue.activated) {
        dependencyInjectionCleaningQueue.activated=false
        treeRoot.addToDependencyInjectionCleaningQueueRecursivelyOnChildren()
      }    
    }
  }
  def unregisterActiveEditor() {
    synchronized {
      activeEditorCount-=1 ; 
      for (l<-activeEditorListeners) l.unregister() ; 
      if (activeEditorCount<=0) { 
        dependencyInjectionCleaningQueue.activated=false ; 
        treeRoot.discardDependenciesRecursivelyOnChildren() 
      } 
    }
  }
  
  
  def dispose() { treeRoot.dispose() }
  //
  // debugging
  //
  
  override def toString = {
    val res = new StringBuilder
    def go(node:XSTreeNode,indent:Int) {
      val c1 = if (node eq currentlyEditing) "*" else " "
      val indentation = " "*indent
      val title = node.toString
      res.append(c1+indentation+node.openChar+" "+title+"\n")
      if (node.isOpen) for (kid<-node.treeChildren) go(kid,indent+1)
    }
    go(treeRoot,0)
    if (res.endsWith("\n")) res.setLength(res.length-1)
    res.toString
  }
  
  //
  // methods applying to the field currently being edited
  //
  
  var currentlyEditing : XSTreeNode = treeRoot
  def changeCurrentlyEditing(newNode:XSTreeNode) { 
    synchronized { 
      def checkParentsOpen(node:XSTreeNode) {
        if (node.parent!=null) {
          checkParentsOpen(node.parent)
          setOpen(node.parent,true)
        }
      }
      checkParentsOpen(newNode)
      currentlyEditing = newNode
      for (p<-detailsPanes) p.setCurrentlyEditing(Some(newNode))
      for (t<-treeListeners) t.setCurrentlyEditing(Some(newNode))
    }
  } 
  
  def setOpen(node:XSTreeNode,open:Boolean) {
    synchronized {
      if (node.isOpen!=open) {
        node.isOpen=open
        broadcast(new TreeChange(List(new TreeNodeChange(node,node.treeChildren,Nil,Nil,Nil)),false))
      }
    }
  }
  //
  // methods applying to actual editing
  //
  
  /** Delete the whole field */
  def deleteTreeNode(node:XSTreeNode) { deleteTreeNodes(List(node),"delete") }

     /** 
      * Delete the whole field for a set of fields. This has to be done carefully in order
      *  > Anything that is a descendent of something to be deleted can be safely ignored.
      *  > If you are deleting multiple things from one parent, they should all be processed together
      *  > If you are deleting things from a parent p, and also from a child c of p, then you need
      *    to be careful. We will first of all just change the parent objects, but not their parents.
      *    This means the tree data structure is somewhat inconsistent, but it is OK as long as we
      *    process the oldest ones first, as the only problem with the inconsistency is the children
      *    list reevaluation. Then we need to resolve fix up all the ancestor nodes, which is done youngest to oldest.
      **/
  def deleteTreeNodes(nodes:Seq[XSTreeNode],undoDescription:String) { synchronized {
    val asSet = nodes.toSet
    if (asSet.contains(treeRoot)) throw new IllegalArgumentException("Cannot delete tree root")
    if (true) { // deal with deletion of currently being edited element - make new currently being edited the first intact parent. 
      var newCurrentlyEditing = currentlyEditing
      while (newCurrentlyEditing.hasSelfOrAncestorInSet(asSet)) newCurrentlyEditing=newCurrentlyEditing.parent
      if (currentlyEditing ne newCurrentlyEditing) changeCurrentlyEditing(newCurrentlyEditing)
    }
    val changes = for ((parent,children)<-nodes.groupBy{_.parent}.toSeq.sortBy{_._1.depth}; if (parent!=null && !parent.hasSelfOrAncestorInSet(asSet))) yield { // don't bother deleting nodes if you are already deleting their parents.
      val newobj = { // needs to deal with the case of subfields of some block becoming part of the block for editing purposes.
        var res = parent.getObject
        for ((fieldInParent,children2)<-children.groupBy{_.fieldInParent}) {
          val indicesToDelete = for (c<-children2) yield c.indexOfFieldInParent
          res=parent.info.deleteFieldAnyRef(res,fieldInParent,indicesToDelete.toSet)
        }
        res
      }
      //val newobj : AnyRef = parent.info.deleteField(parent.obj,node.fieldInParent,node.obj) 
      val changes = parent.changeObject(newobj)
      assert (changes.removedChildren.length==children.filter{!_.fieldInParent.isTableOrInlineEditable}.length)
      assert (changes.addedChildren.isEmpty)
      assert (changes.sub.isEmpty)
      changes
    }
    processChangesToKids(changes,null,undoDescription)
  }}

  
  /** 
   * We have had a set of changes to some nodes. The data structures are now inconsistent, in that 
   * some nodes (the parents in the ChildrenChange elements) have objects that are no longer the children
   * of their parents. We need to fix this. Do this by updating all the ancestors. To prevent multiple changes
   * to one node (which would cause a problem with the inconsistent data structure), 
   * we need to process the deepest ones first.
   * 
   * This function makes the structure consistent (by modifying ancestors), and then tells listeners about
   * the changes once everything is consistent.
   */
  def processChangesToKids(changes:Seq[TreeNodeChange],undoRedoKey:AnyRef,undoDescription:String) { synchronized {
    val fullList = new ListBuffer[TreeNodeChange]
    fullList++=changes
    var haveChangedObject : Set[XSTreeNode] = changes.map{_.parent}(collection.breakOut)
    while (!haveChangedObject.isEmpty) {
      val deepest : Int = haveChangedObject.map{_.depth}.toSeq.max
      val (donow,defer) = haveChangedObject partition {_.depth==deepest}
      haveChangedObject = defer
      if (deepest>0) { // if = 0, then processing parentless node.
        for ((parent,children)<-donow.groupBy{_.parent}) {
          var res = parent.getObject
          for (c<-children) res=parent.info.changeFieldAnyRef(res,c.indexOfFieldInParent,c.fieldInParent,c.getObject)
          val changes = parent.changeObject(res)
          assert (changes.removedChildren.isEmpty)
          assert (changes.addedChildren.isEmpty)
          assert (changes.sub.isEmpty)
          if (!haveChangedObject.contains(parent)) { haveChangedObject+=parent; fullList+=changes; }
        }
      }
    }
    if (undoDescription!=null) undoRedo.addUserChange(currentObject,undoRedoKey,undoDescription)
    broadcast(new TreeChange(fullList.toList,true))
    updateToolbar()
    dependencyInjectionCleaningQueue.cleanReturningInstantlyIfSomeOtherThreadIsAlreadyCleaning()
  }}

  def broadcast(changes:TreeChange) {
    for (l<-treeListeners) l(changes)
    for (p<-detailsPanes) p.refresh(changes)
  }
  
  private var isIntrinsiclyDirty = false
  def setIntrinsiclyDirty(newIsIntrinsiclyDirty:Boolean) { isIntrinsiclyDirty=newIsIntrinsiclyDirty;updateToolbar() }
  private var isStale = false
  def setIsStale(newIsStale:Boolean) { isStale=newIsStale;updateToolbar() }
  def isDirty = undoRedo.canUndo.isDefined || isIntrinsiclyDirty
  def getToolbarStatus = new StatusForToolbar(undoRedo.canUndo,undoRedo.canRedo,isDirty,isStale)
  def updateToolbar() {
    val status = getToolbarStatus
    for (l<-toolbarStatusListeners) l(status)
  }
  /**
   * Moving can come from a variety of sources:
   *  (1) Copy/Paste - makes a copy, stuck on end
   *  (2) Cut/Paste - removes old (possibly from different data structure), inserts new stuck on end
   *  (3) Drag 'n drop - possibly removes from original, inserts new stuff at a particular place.
   */

  /**
   * Add a new field "element" of the stated type "field" to the node "parent". If "before" is empty, it will be added at the end.
   * Otherwise it will be added before "before"
   */
  def addField(parent:XSTreeNode,before:Option[XSTreeNode],field:XSFieldInfo,element:AnyRef,executeAfterModificationBeforeRefreshing:Option[()=>Unit],undoDesc:String) {
    val newobj = parent.info.addFieldAnyRef(parent.getObject,before.map{_.numberOfElementsBeforeThisOneOfGivenType(field)},field,element).asInstanceOf[AnyRef]
    // println("New object in add field "+newobj)
    val changes = parent.changeObject(newobj)
    assert (changes.removedChildren.isEmpty)
    assert (field.isTableEditable || field.isInlineEditable || !changes.addedChildren.isEmpty) 
    assert (changes.sub.isEmpty)
    for (f<-executeAfterModificationBeforeRefreshing) f()
    processChangesToKids(List(changes),null,undoDesc)
    for (newnode<-changes.addedChildren.find{_.getObject eq element}) changeCurrentlyEditing(newnode)
  }

  /**
   * Set a field to a new value. If the field is a collection, then so should be newValue.
   */
  def setField(parent:XSTreeNode,field:XSFieldInfo,newValue:AnyRef,executeAfterModificationBeforeRefreshing:Option[()=>Unit]) {
    val newobj = parent.info.setFieldAnyRef(parent.getObject,field,newValue)
    val changes = parent.changeObject(newobj)
    for (f<-executeAfterModificationBeforeRefreshing) f()
    processChangesToKids(List(changes),(parent,field),"change field")
  }
  
  /**
   * Wholesale change to a node. Useful for custom editors.
   */
  def changeNode(parent:XSTreeNode,newobj:AnyRef,executeAfterModificationBeforeRefreshing:Option[()=>Unit],undoDescription:String) {
    val changes = parent.changeObject(newobj)
    for (f<-executeAfterModificationBeforeRefreshing) f()
    processChangesToKids(List(changes),(parent,undoDescription),undoDescription)
  }
  
  def copyData(nodes:Seq[XSTreeNode]) : XSClipBoard = {
    val out = new ByteArrayOutputStream
    val writer:XMLStreamWriter = XMLSerialize.outputFactory.createXMLStreamWriter(out,"UTF-8");
    { // write document header
      writer.writeStartDocument("UTF-8","1.0");
      writer.writeCharacters("\n");
	}
    writer.writeStartElement(XMLSerialize.CopiedDataTag) 
    for (n<-nodes) n.serialize(writer)
    writer.writeEndElement();
	writer.writeEndDocument();
	writer.close();
	XSClipBoard.serialized(out.toByteArray())
  }
  
  /**
   * Add a new fields given in the serialized data to the node "parent". If "before" is empty, it will be added at the end.
   * Otherwise it will be added before "before"
   */
  def pasteData(parent:XSTreeNode,data:XSClipBoard,before:Option[XSTreeNode],undoDesc:String="paste") {
    val loadBefore = for (b<-before) yield (b.fieldInParent,b.numberOfElementsBeforeThisOneOfGivenType(b.fieldInParent))
    val reader = XMLDeserialize.inputFactory.createXMLStreamReader(new ByteArrayInputStream(data.data),"UTF-8");
    val (newobj,openNodes) = parent.info.deserializeInto(reader,parent.getObject,loadBefore)
    val changes = parent.changeObject(newobj.asInstanceOf[AnyRef])
    for (c<-changes.addedChildren) c.setOpenNodes(openNodes)
    processChangesToKids(List(changes),null,undoDesc)
  }
  
  def dragData(destination:XSTreeNode,source:Seq[XSTreeNode],before:Option[XSTreeNode]) {
    val asSet = source.toSet
    if (destination.hasSelfOrAncestorInSet(asSet)) throw new IllegalArgumentException("Cannot drag onto self")
    //val safebefore = before match {
    //  case Some(b) if asSet.contains(b) => b.parent.children.dropWhile{_ ne b}.find{!asSet.contains(_)} // get next child not being deleted.
    //  case _ => before
    //}
    val data = copyData(source)
    pasteData(destination,data,before,null) // this may error. Do this before deleting nodes in case exception gets thrown - deleting but not reinserting would be bad for a user.
    deleteTreeNodes(source,"drag")
  }
  
  def replaceRoot(newval:AnyRef) {
    synchronized {
      undoRedo.reset(newval)
      changeRootTo(newval)
    }
  }
  
  private def changeRootTo(newval:AnyRef) {
    synchronized {
      val change = treeRoot.changeObject(newval)
      var newCurrentlyEditing = currentlyEditing
      while (!newCurrentlyEditing.isStillBeingEdited) newCurrentlyEditing=newCurrentlyEditing.parent
      if (newCurrentlyEditing ne currentlyEditing) changeCurrentlyEditing(newCurrentlyEditing)
      broadcast(new TreeChange(List(change),true))
      updateToolbar()
      
    }
    dependencyInjectionCleaningQueue.cleanReturningInstantlyIfSomeOtherThreadIsAlreadyCleaning()
  }
  
  /** return true iff work */
  def undo() : Boolean = {
    undoRedo.undo() match {
      case Some(newval) => changeRootTo(newval); true 
      case None => false
    }
  }
  
  /** return true iff work */
  def redo() : Boolean = {
    undoRedo.redo() match {
      case Some(newval) => changeRootTo(newval); true 
      case None => false
    }
  }
  
  def getTitle(locale:Locale) : Option[String] = treeRoot.info.textResources(locale).get("PageTitle")

  /** 
   *  Provide extra information to the dependency injection system. This will be provided to the root node and any children (other unless blocked with @BlockDependencyInjection).
   *  Typically used for external information about the document (e.g. filename), path, user preferences.
   */
  def setRootDependencyInjections(toInject:Set[AnyRef]) {
    treeRoot.dependencyInjection.changedParentInjections(toInject)
    dependencyInjectionCleaningQueue.cleanReturningInstantlyIfSomeOtherThreadIsAlreadyCleaning()
  }
    // do this last of all
  
  dependencyInjectionCleaningQueue.cleanReturningInstantlyIfSomeOtherThreadIsAlreadyCleaning()
}

class TreeChange(val elements:Seq[TreeNodeChange],/** True iff the actual data has changed */ val dataChanged:Boolean) {
  /** Elements, plus all subs */
  lazy val elementsIncludingRecursive : List[TreeNodeChange] = {
    val res = new ListBuffer[TreeNodeChange]
    def add(e:TreeNodeChange) { res+=e; for (s<-e.sub) add(s) }
    for (s<-elements) add(s)
    res.toList
  }
  def contains(node:XSTreeNode) : Boolean = elementsIncludingRecursive.exists{_.parent==node} 
  override def toString = elements.mkString(";")
}


trait XSEditListener {
  def apply(changes:TreeChange) : Unit
  def setCurrentlyEditing(node:Option[XSTreeNode])
}

trait ToolbarStatusListener {
  def apply(status:StatusForToolbar)
}

/** Something that wants to see when the number of active clients using this XSEditor changes */
trait ActiveEditorRegistrationListener {
  def register()
  def unregister()
}



class StatusForToolbar(val undoDesc:Option[String],val redoDesc:Option[String],val dirty:Boolean,val stale:Boolean)
