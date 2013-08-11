/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.frontend

import org.greatcactus.xs.impl.SerializableTypeInfo
import org.greatcactus.xs.impl.XSFieldInfo
import org.greatcactus.xs.api.errors.XSError
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import javax.xml.stream.XMLStreamWriter
import org.greatcactus.xs.api.serialization.XMLSerialize
import org.greatcactus.xs.util.EqualityByPointerEquality
import scala.collection.mutable.SetBuilder
import org.greatcactus.xs.util.EqualityByPointerEquality
import org.greatcactus.xs.api.icon.Icon
import org.greatcactus.xs.impl.DependencyInjectionCurrentStatus
import org.greatcactus.xs.impl.DependencyInjectionFunction
import java.util.Locale
import org.greatcactus.xs.api.display.RichLabel
import org.greatcactus.xs.api.errors.ResolvedXSError
import org.greatcactus.xs.api.icon.ConcreteIcon
import org.greatcactus.xs.impl.TrimInfo
import org.greatcactus.xs.util.InvalidatableCache

/**
 * Contain information about the hierarchical structure of an XS object, suitable for displaying in a JTree or similar.
 * 
 * <p>This mutable structure contains the tree representation of the actual immutable object. The idea
 * is that editing changes the object significantly, but as much of the tree is left intact as possible,
 * so that GUIs don't have things closing unexpectedly. This also keeps track of dependency injections and
 * errors.</p>
 * 
 * The basic operations that can be done during an edit are
 *   -> change a field (which will propagate through to its parents)
 *   -> move a field to elsewhere in the tree (it does not bother to try to keep track of the open status of the thing being moved)
 *   -> delete or insert a field
 */
class XSTreeNode(
    /** Null if there is no parent */
    val parent:XSTreeNode,
    val info : SerializableTypeInfo[_], 
    /** What field this element is in the parent */
    val fieldInParent : XSFieldInfo,
    /** The object at this level */
    private[this] var obj : AnyRef,
    val xsedit:XSEdit,
    /** Correct at the time of creation, but not afterwards. Can't just use indexOfFieldInParent as parent's children will be be initialized yet.  */
    indexInParentFieldAtTimeOfCreation:Int
    ) {
  /** Unique amongst all nodes for a given edit structure */
  val uid : Long = xsedit.uidsForTreeNodes.newID()
  def injectionNodesFromParent : Set[AnyRef] = if (parent==null) xsedit.globalDependencyInjections else parent.dependencyInjection.dependenciesToPropagateToChildren(info.dependencyInjectionInfo.fromParentDependencyInfo,indexInParentFieldAtTimeOfCreation)
  private[xs] val dependencyInjection = new DependencyInjectionCurrentStatus(info.dependencyInjectionInfo,this)
  dependencyInjection.changedObject(null,obj)
  @volatile private[this] var disposed=false
  var isOpen : Boolean = if (fieldInParent==null) true else fieldInParent.isExpandOnFirstDisplay
  
  def isStillBeingEdited : Boolean = isRoot || (parent.isStillBeingEdited && parent.treeChildren.contains(this))
  
  def isRoot = parent==null
  val depth:Int = if (parent==null) 0 else 1+parent.depth
  private[this] var treeChildrenV : IndexedSeq[XSTreeNode] = getTreeChildren(Nil).children
  private[this] var tableChildrenV : Map[XSFieldInfo,IndexedSeq[XSTreeNode]] = getTableChildren(Map.empty++(info.tableAndInlineNodeFields.map{_ -> IndexedSeq.empty}))
  def treeChildren : IndexedSeq[XSTreeNode] = treeChildrenV
  def tableAndInlineChildren : Map[XSFieldInfo,IndexedSeq[XSTreeNode]] = tableChildrenV
  def allChildren : IndexedSeq[XSTreeNode] = treeChildrenV++tableAndInlineChildren.values.flatten
  def root : XSTreeNode = if (parent==null) this else parent.root
  /** If this is a table line, rather than a tree line */
  private var tableLine : Option[(XSFieldInfo,Int)] = None
  def isTableLine : Boolean = tableLine.isDefined
  def getTableLine : Option[(XSFieldInfo,Int)] = tableLine
  def isDescendentOf(node:XSTreeNode) : Boolean = if (parent==node) true else if (parent==null) false else parent.isDescendentOf(node)
  
  def getTreeChildren(oldTreeChildren:Seq[XSTreeNode]) : TreeNodeChange = getChildren(info.treeNodeFields,oldTreeChildren)
  def getTableChildren(oldTableChildren:Map[XSFieldInfo,IndexedSeq[XSTreeNode]]) : Map[XSFieldInfo,IndexedSeq[XSTreeNode]] = {
    for ((tableField,oldkids)<-oldTableChildren) yield {
      val newkids = getChildren(List(tableField),oldkids).children
      for (i<-0 until newkids.length) newkids(i).tableLine=Some((tableField,i))
      (tableField,newkids)
    }
  }
    
  def getDuplicateStrings(strings:Seq[String]) : Set[String] = {
    var res : Set[String] = Set.empty
    var found : Set[String] = Set.empty
    for (s<-strings) if (found.contains(s)) res+=s else found+=s
    res
  }
  
  private def getChildren(fields:Seq[XSFieldInfo],oldChildren:Seq[XSTreeNode]) : TreeNodeChange = {
    val allChildren = new ArrayBuffer[XSTreeNode]
    val addedChildren = new ListBuffer[XSTreeNode]
    val deletedChildren = new ListBuffer[XSTreeNode]
    val subTreeNodeChanges = new ListBuffer[TreeNodeChange]
    // children match by equality of the obj elements
    for (blockField<-fields) {
      val rawContents : Array[AnyRef] = (for (elem<-blockField.getAllFieldElements(obj); if (elem!=null)) yield elem.asInstanceOf[AnyRef]).toArray // the actual user data structure values we are trying to match
      val oldChildrenThisField : Seq[XSTreeNode] = oldChildren.filter{_.fieldInParent eq blockField}
      // the new children that match them
      val foundChildren : Array[XSTreeNode] = new Array[XSTreeNode](rawContents.length) // note that null elements of this array may be improved later
      def imperfectMatch(index:Int,node:XSTreeNode) {
        foundChildren(index)=node;
        subTreeNodeChanges+=node.changeObject(rawContents(index)) 
      }
      // first try to match by object equality
      val (failed1indices,failed1nodes) = {
        var pointerEqualityMap : Map[EqualityByPointerEquality[AnyRef],XSTreeNode]= Map.empty++(for (c<-oldChildrenThisField) yield new EqualityByPointerEquality(c.getObject)->c)
        val failPointerEqualityBuffer = new ListBuffer[Int]
        for (i<-0 until rawContents.length) {
          val holder = new EqualityByPointerEquality(rawContents(i))
          pointerEqualityMap.get(holder) match {
            case Some(node) => foundChildren(i)=node; pointerEqualityMap-=holder;
            case None => failPointerEqualityBuffer+=i
          }
        }    
        (failPointerEqualityBuffer.toList,pointerEqualityMap.values.toSeq)
      }
      // second try to match by perfect unique toString equality
      val failed2indices = {
        val failStringEqualityBuffer = new ListBuffer[Int]
        val duplicateStrings = getDuplicateStrings(failed1indices.map{i=>rawContents(i).toString})++ getDuplicateStrings(failed1nodes.map{_.getObject.toString}.toSeq)
        val perfectStringMatch : Map[String,XSTreeNode] = Map((for (v<-failed1nodes;s=v.getObject.toString;if !duplicateStrings.contains(s)) yield s->v) :_*)
        for (i<-failed1indices) perfectStringMatch.get(rawContents(i).toString) match {
            case Some(node) if node.getObject.getClass == rawContents(i).getClass => imperfectMatch(i,node) // need to check class equality in case the field is polymorphic and two subclasses have the same toString.  
            case _ => failStringEqualityBuffer+=i          
        }
        failStringEqualityBuffer.toList
      }
      val usedOldChildren : Set[EqualityByPointerEquality[XSTreeNode]] = foundChildren.filter{_!=null}.map{new EqualityByPointerEquality(_)}(collection.breakOut) 
      var unusedOldChildren : List[XSTreeNode] = oldChildrenThisField.filter{n=> !usedOldChildren.contains(new EqualityByPointerEquality(n))}.toList
      // third try just match order. If that fails, make a new one.
      val _ = {
        for (i<-failed2indices) unusedOldChildren match {
          case node::t if node.getObject.getClass == rawContents(i).getClass => imperfectMatch(i,node); unusedOldChildren=t // need to check class equality in case the field is polymorphic and two subclasses have the same toString. 
          case _ => // run out of options. Make a new one.
            XSTreeNode.apply(rawContents(i),this,blockField,xsedit,i) match {
              case Some(node) => foundChildren(i)=node; addedChildren+=node 
              case None => // can't edit this
            }
        }
      }
      deletedChildren++=unusedOldChildren
      for (c<-foundChildren) if (c!=null) allChildren+=c
    }
    new TreeNodeChange(this,allChildren.toIndexedSeq,deletedChildren.toList,addedChildren.toList,subTreeNodeChanges.toList)
  }

  
  
  /** If this is the nth element of a given field in the parent, return n (0 based). */
  private[frontend] def indexOfFieldInParent : Int = numberOfElementsBeforeThisOneOfGivenType(fieldInParent)

  /** Get the number of elements of the stated type that occur before this element in the tree.  */
  private[frontend] def numberOfElementsBeforeThisOneOfGivenType(field:XSFieldInfo) : Int = {
    if (parent==null) 0
    else {
      // can't just loop over field.getAllFieldElements(parent.getObject) as there may be multiple equal (pointer equality too) fields in a collection.
      var res = 0
      for (n<-if (field.isTableEditable || field.isInlineEditable) parent.tableAndInlineChildren(field) else parent.treeChildren) {
        if (n eq this) return res
        else if (n.fieldInParent==field) res+=1
      }
      res
    }
  }
  
  def hasSelfOrAncestorInSet(set:Set[XSTreeNode]) : Boolean = {
    if (set.contains(this)) true
    else if (parent==null) false
    else parent.hasSelfOrAncestorInSet(set)
  }
  

  /** 
   * Child objects should be changed before the parent objects. Otherwise when the class tries to match child objects
   * to existing child XSTreeNode elements, it will not necessarily get a perfect mapping between children and tree
   * nodes (although it will try)
   **/
  def changeObject(newobj : AnyRef) : TreeNodeChange = { 
    synchronized {
      uniquenessCheckResolution.invalidate()
      tableFieldsCache.clear()
      worstErrorLevelCache.invalidate()
      dependencyInjection.changedObject(obj,newobj)
      obj = newobj
      val kids = getTreeChildren(treeChildren)
      kids.disposeRemoved()
      treeChildrenV = kids.children
      tableChildrenV = getTableChildren(tableChildrenV)
      kids
    }
  }
    
  /** Disposal needs to be done to dispose of the listeners in the dependencyInjection structure */
  def dispose() {
    if (disposed) return // could possible throw an error, at least in dev mode.
    synchronized {
      disposed=true
      for (c<-allChildren) c.dispose()
      dependencyInjection.dispose()
    }
  }
  
  /** Note - this may be slow. Gets the current dependency injection information for this node */
  def blockingGetDependencyInjectionInformation() : Set[AnyRef] = {
    if (parent!=null) parent.blockingGetDependencyInjectionInformation()
    cleanDependencies()
    dependencyInjection.lastInjections 
  }
  def cleanDependencies() {
    if (disposed) return
    uniquenessCheckResolution.clean()
   // println("In cleanDependencies for "+this)
    if (dependencyInjection.clean()) { // need to refresh this node on clients.
      tableFieldsCache.clear()
      updateGUIincludingErrorLevels()
    }
    if (disposed) dependencyInjection.discardDependencies()
  }
  
  def discardDependencies() { dependencyInjection.discardDependencies() }
  def discardDependenciesRecursivelyOnChildren() { 
    synchronized {
      discardDependencies()
      for (c<-allChildren) c.discardDependenciesRecursivelyOnChildren()
    }
  }
  def addToDependencyInjectionCleaningQueueRecursivelyOnChildren() {
    synchronized {
      xsedit.dependencyInjectionCleaningQueue.add(this)
      for (c<-allChildren) c.addToDependencyInjectionCleaningQueueRecursivelyOnChildren()
    }    
  }
  
  def updateGUIincludingErrorLevels() {
      worstErrorLevelCache.invalidate()
      if (parent!=null) parent.childHadWorstErrorLevelRecomputed()
      //println("Boadcasting clean event for "+this)
      updateGUI()    
  }

  def updateGUIIncludingTableFieldsCache() {
      tableFieldsCache.clear()
      updateGUI()
  }
  
  def updateGUI() {
    xsedit.broadcast(new TreeChange(List(new TreeNodeChange(this,treeChildren,Nil,Nil,Nil)),false))
  }
  
  private def childHadWorstErrorLevelRecomputed() {
    val changed = worstErrorLevelCache.synchronized {
       val existing = worstErrorLevelCache.get
       worstErrorLevelCache.invalidate()
       val newRes = worstErrorLevelCache.get
       existing!=newRes 
    }
    if (changed) {
      updateGUI()
      if (parent!=null) parent.childHadWorstErrorLevelRecomputed()
    }
  }
  
  private def getAllOpenNodes(openTags:ListBuffer[EqualityByPointerEquality[AnyRef]]) {
    if (isOpen) openTags+=new EqualityByPointerEquality(obj)
    for (c<-treeChildren) c.getAllOpenNodes(openTags)
  } 
  private[frontend] def setOpenNodes(openTags:Set[EqualityByPointerEquality[AnyRef]]) {
    isOpen = openTags.contains(new EqualityByPointerEquality(obj))
    for (c<-treeChildren) c.setOpenNodes(openTags)
  }
  
  /** Serialize this fragment to the writer */
  def serialize(writer:XMLStreamWriter) {
    val openTags = new ListBuffer[EqualityByPointerEquality[AnyRef]]
    getAllOpenNodes(openTags)
    XMLSerialize.serialize(obj,writer,info,if (fieldInParent==null) None else fieldInParent.overridingName,openTags.toSet)
  }
  
  def label(locale:Locale):RichLabel = RichLabel(dependencyInjection.getLabel(this),obj.toString,locale)
  def tooltip(locale:Locale):Option[RichLabel] = dependencyInjection.getTooltip(this).flatMap{RichLabel(_,locale)}
  
  private val iconFromDependencies : AnyRef=>Option[Icon] = {
    case s:String => info.iconSource.iconOfLogicalName(s)
    case i:Icon => Some(i)
    case i:ConcreteIcon => Some(new Icon("",List(i)))
    case _ => None    
  }
  
  def icon:Option[Icon] = dependencyInjection.getIconSpec(this).flatMap{iconFromDependencies}.orElse(info.icon)
  
  /** Get the icon for the field from dependency injection */
  def specialIconForField(fieldname:String) : Option[Icon] = dependencyInjection.getIconSpecForField(fieldname,this).flatMap{iconFromDependencies}
  
  /** Get the label for the field from dependency injection */
  def specialLabelForField(fieldname:String,locale:Locale):Option[RichLabel] = dependencyInjection.getLabelForField(fieldname,this).flatMap{RichLabel(_,locale)}
  def tooltipForField(fieldname:String,locale:Locale): Option[RichLabel] = dependencyInjection.getTooltipForField(fieldname,this).flatMap{RichLabel(_,locale)}
  
  def errors(fieldname:String,locale:Locale,humanEdited:Array[Option[TrimInfo]]) : List[ResolvedXSError] = {
    lazy val collectionLengths = for (field<-info.fields.find(_.name==fieldname);col<-field.getFieldAsStringCollectionLengthInfo(obj,humanEdited)) yield col 
    dependencyInjection.getErrors(fieldname,this).map{_.resolve(locale, collectionLengths)}
  }
  private[this] val worstErrorLevelCache = new InvalidatableCache[Int](
      allChildren.foldLeft(dependencyInjection.worstErrorLevel(this))((e,n)=>e.min(n.worstErrorLevel))
    
  )
  def worstErrorLevel : Int = worstErrorLevelCache.get
  
  def getPseudoField(function:DependencyInjectionFunction,locale:Locale) : RichLabel = RichLabel(dependencyInjection.getFunctionResult(function,this),"",locale)
  
  private[this] val tableFieldsCache =new collection.mutable.HashMap[ColumnExtractors,IndexedSeq[String]]
  
  def getTableFields(extractor:ColumnExtractors) : IndexedSeq[String] = tableFieldsCache.getOrElseUpdate(extractor,extractor.extract(this))
  
  def mayDelete = parent!=null && fieldInParent.isCollectionOrArray
  def isEnabled(field:String) = dependencyInjection.isEnabled(field,this)
  def isVisible(field:String) = dependencyInjection.isVisible(field,this)
  def canAdd(field:XSFieldInfo) = field.maxChildren match {
    case -1 => true
    case n =>
      val existing = field.getAllFieldElements(getObject).size
      // println("canAdd: Field "+field.name+" n="+n+" existing="+existing)
      existing < n
  }

  

  def openChar : String = if (treeChildren.isEmpty) "." else if (isOpen) "-" else "+"
  def toFullTreeString(indent:Int,locale:Locale):String = {
    val self = " "*indent+openChar+" "+label(locale).text  // TODO include tooltip(locale)
    val kids = treeChildren.map{_.toFullTreeString(indent+1,locale)}
    (self::kids.toList).mkString("\n")
  } 
  override def toString = obj.toString
  
  def getObject = obj
  
  //
  // code for handling uniqueness annotations
  //
  
  val uniquenessCheckResolution = new org.greatcactus.xs.impl.UniquenessCheckResolution(this,info.uniquenessCheckLocal,info.uniquenessCheckParent,info.uniquenessCheckGlobal)
  def parentUniquenessErrorsChanged() {
    //println("parentUniquenessErrorsChanged for "+this)
    if (!uniquenessCheckResolution.parent.isEmpty) { worstErrorLevelCache.invalidate(); dependencyInjection.changedUniquenessValues(uniquenessCheckResolution.parent) }
  }
  
  def globalUniquenessErrorsChanged() {
    //println("globalUniquenessErrorsChanged for "+this)
    if (!uniquenessCheckResolution.global.isEmpty)  { worstErrorLevelCache.invalidate(); dependencyInjection.changedUniquenessValues(uniquenessCheckResolution.global) }
    for (c<-allChildren) c.globalUniquenessErrorsChanged()
  }
  
  
  xsedit.dependencyInjectionCleaningQueue.add(this)
}

class TreeNodeChange(val parent:XSTreeNode,val children:IndexedSeq[XSTreeNode],val removedChildren:List[XSTreeNode],val addedChildren:List[XSTreeNode],val sub:List[TreeNodeChange]) {
  def changedStructure = !(removedChildren.isEmpty&&addedChildren.isEmpty)
  
  private[xs] def disposeRemoved() { for (gone<-removedChildren) gone.dispose() }
  override def toString = parent.toString
}

//class ErrorInTree(val error:XSError,val field:Option[XSFieldInfo])

// class PathToParent(val path:List[XSTree])

object XSTreeNode {
  /** Get an XSTree for the given object, which should be @XS */
  def apply(obj:AnyRef,xsedit:XSEdit) : XSTreeNode = {
     apply(obj,null,null,xsedit,0).getOrElse(throw new IllegalArgumentException("Object class "+obj.getClass+" is not serializable by XS"))
  }
  
  private def apply(obj:AnyRef,parent:XSTreeNode,fieldInParent:XSFieldInfo,xsedit:XSEdit,indexInParentField:Int) : Option[XSTreeNode] = {
    for (info<-SerializableTypeInfo.get(obj.getClass)) yield {
      val res = new XSTreeNode(parent,info,fieldInParent,obj,xsedit,indexInParentField)
      //res.treeChildren = for (blockField<-info.fieldsAsBlocks;elem<-blockField.getAllFieldElements(obj);subtree<-apply(elem.asInstanceOf[AnyRef],res,blockField)) yield subtree
      res
    }
  }
}

