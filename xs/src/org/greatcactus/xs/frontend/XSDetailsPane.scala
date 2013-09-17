/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.frontend

import java.util.Locale
import scala.xml.Attribute
import scala.xml.Text
import scala.xml.NodeSeq
import scala.collection.mutable.ListBuffer
import org.greatcactus.xs.api.display.RichLabel
import org.greatcactus.xs.api.icon.Icon
import org.greatcactus.xs.api.errors.ResolvedXSError
import org.greatcactus.xs.impl.XSFieldInfo
import scala.util.Success
import scala.util.Failure
import org.greatcactus.xs.impl.TrimInfo
import org.greatcactus.xs.impl.CollectionStringUtil
import scala.concurrent._
import scala.collection.mutable.ArrayBuffer
import org.greatcactus.xs.impl.GeneralizedField
import org.greatcactus.xs.api.command.ProgressMonitor
import org.greatcactus.xs.api.command.ProgressMonitorUI
import org.greatcactus.xs.impl.SerializableTypeInfo
import scala.xml.Node
import org.greatcactus.xs.api.command.EditCommandDescription
import org.greatcactus.xs.api.command.EditCommandDescription
import org.greatcactus.xs.api.command.EditCommandDescriptionMadeConcrete

/**
 * A GUI client should extend this controller to manage the "details" pane of the item currently being edited.
 * 
 * It will get events flowing to it (change of what object is being edited from XS, change of details from whatever UI elements it has produced). This keeps track of what is currently being shown, and the contents of its fields.
 * 
 * There will be one copy of this per client
 *
 */
abstract class XSDetailsPane[T](val locale:Locale,val xsedit:XSEdit,val executionContext:ExecutionContext) { basePane =>

  private var currentlyShowing : Option[XSTreeNode] = None
  private var currentPane : Option[DetailsPaneFields] = None
  private var currentUIElements : Option[UIFields] = None
  
  def getCurrentlyShowing = currentlyShowing
  def getCurrentPane = currentPane
  def getCurrentUIElements = currentUIElements
  
  def dispose() {
    synchronized {
      for (gui<-currentUIElements) dispose(gui)
      currentUIElements=None
    }
  }

  /** Called by XSEdit when the thing currently being show has changed. */
  def setCurrentlyEditing(newnode:Option[XSTreeNode]) {
    synchronized {
      if (currentlyShowing!=newnode) {
        currentlyShowing=newnode
        newnode match {
          case Some(t) =>
            setNodeIDCurrentlyBeingEdited(t.uid.toString)
            val newPane = t.info.getPane(locale,t.mayDelete)
            val wrappedNewPane = Some(newPane)
            if (currentPane!=wrappedNewPane) {
              dispose()
              currentPane=wrappedNewPane
              currentUIElements=Some(makeNewGUI(t,newPane))
            } else userHasAbandonedNoncanonicalEdits()
            refresh() // needs to be done even if a new GUI is made to update the errors.
          case None =>
            setNodeIDCurrentlyBeingEdited(null)
            dispose()
            currentPane = None
        }
        flushClientCommands()
      }
    }
  }

  var nodeIDCurrentlyBeingEdited:String = null
  /**
   * For clients with a delay between the client and the server (eg web browser), then there is the problem of
   * what if the client does some action (eg make a new field), and then, before the client is correctly updated,
   * does some new action believing it to be in the same context. Then by the time it reaches the server, the server
   * thinks the context is different. A similar problem occurs if the user presses some action twice if it didn't work
   * immediately the first time.
   * 
   * The XS resolution is to send a token indicating what is currently being edited. Context sensitive user actions
   * will then send back the token with them. The server will then ignore commands if the token from the client
   * does not match what the server expects.
   */
  def setNodeIDCurrentlyBeingEdited(id:String) {
    nodeIDCurrentlyBeingEdited=id
  }
  
  
  //
  // Commands that the client send to XS (triggered by UI actions by the user)
  //
  
  /** The client should call this when an action has been triggered by the user. The argument is the GUI item that has been triggered. */
  def uiActivated(uiElement:T,sequence:Int) {
    uiAction(uiElement,{
      case f:UIFieldAction => node => f.go(node)
      case f:UIFieldEditCommands => node => f.go(sequence,node)
    })
  }
  
  /** The client should call this when a gui element represented by a string has changed its value */
  def uiChangedTextField(uiElement:T,changedTo:String,finalChange:Boolean) {
    //println("Changing field "+uiElement+" to "+changedTo)
    //println("Field "+uiField(uiElement))
    uiAction(uiElement,{
      case f:UIFieldText => node => f.changedByGUI(node,changedTo)
      case f:UIFieldImage => node => f.changedByGUI(node, changedTo)
    })
  }
  
  /** The client should call this when a gui element represented by a boolean has changed its value */
  def uiChangedBooleanField(uiElement:T,changedTo:Boolean) {
    uiAction(uiElement,{
      case f:UIFieldBoolean => node => f.changedByGUI(node,changedTo)
    })
  }

  /** The client should call this when a gui element represented by a grid makes a new row by editing a field */
  def uiNewRowOnGrid(uiElement:T,columnName:String,newValue:String) {
    uiAction(uiElement,{
      case f:UIFieldTable => node => f.addedNewRow(node,columnName,newValue)
    })    
  }
  /** The client should call this when a gui element represented by a grid changes a cell entry. */
  def uiChangeGrid(uiElement:T,rowNumber:Int,columnName:String,newValue:String) {
    uiAction(uiElement,{
      case f:UIFieldTable => node => f.changedElement(node,rowNumber,columnName,newValue)
    })
  }
  /** The client should call this when a gui element represented by a grid does a table paste at a particular cell. */
  def uiPasteGrid(uiElement:T,rowNumber:Int,columnName:String,table:Array[Array[String]]) {
    uiAction(uiElement,{
      case f:UIFieldTable => node => f.pasteTable(node,rowNumber,columnName,table)
    })
  }
  
  /** The client should call this when a gui element represented by a grid reorders rows by drag and drop or similar */
  def uiDragGridRows(uiElement:T,rowsToBeMoved:Seq[Int],insertBefore:Int) {
    uiAction(uiElement,{
      case f:UIFieldTable => node => f.uiDragGridRows(node,rowsToBeMoved,insertBefore)
    })    
  }

  def uiTableContextMenu(uiElement:T,command:String,selectedRows:Seq[Int]) {
    uiAction(uiElement,{
      case f:UIFieldTable => node => f.uiTableContextMenu(node,command,selectedRows)
    })        
  }
  /** The client should call this when the user clicks on a cancel button for a long running command */
  def uiCancelJob(uiElement:T) {
    uiAction(uiElement,{
      case f:UIFieldAction => node => f.cancel()
      case g => node => println("Cancel else "+g)
    })        
  }
  def uiInitiatePopup(uiElement:T) {
    uiAction(uiElement,{
      case f:UIFieldText => node => for (popupName<-f.field.field.customPopup) initiatePopup(f,popupName,node)
      case g => node => ()
    })        
  }
  def uiPopupSetField(uiElement:T,newvalue:String) {
    uiAction(uiElement,{
      case f:UIFieldText => node => f.field.set(node,newvalue,None); f.refresh(node); flushClientCommands()
      case g => node => ()
    })        
  }
  /** perform the given action on the uiElement */
  private def uiAction(uiElement:T,action:PartialFunction[UIField,XSTreeNode=>Unit]) {
    synchronized {
     try {
      for (node<-currentlyShowing) {
        for (elem<-uiField(uiElement); realnode<-elem.submap.map(node)) 
          if (action.isDefinedAt(elem.field)) action(elem.field)(realnode)
      }
     } catch { case e:Exception => e.printStackTrace() } // make sure that XS doesn't crash the front end code.
    }
  }

  //
  // Commands XS sends to the client
  //
  /** Client code to set up a custom popup */
  def initiatePopup(field:UIFieldText,popupName:String,node:XSTreeNode)
  /** Client code to dispose of the GUI elements created previously */
  def dispose(guis:UIFields) : Unit
  /** Client code to remove a GUI element. Used for inline edit blocks. Elements should be disposed before calling this. */
  def remove(gui:T) : Unit
  /** Called when the screen should be set to blank */
  def setBlankScreen() : Unit
  /** Possibly overridden method that says make sure everything is sent to the client. Useful for clients (like HTML) that may bunch multiple commands up into one packet. */
  def flushClientCommands()
  /** Create a new object for generating the GUI elements for a new screen */  
  def newCreator() : GUICreator[T]
  /** Get a custom painter for a given GUI element */
  def getCustom(f:DetailsPaneFieldCustom) : Option[CustomComponent[_,T]]
  /** XS sending a command to the GUI to change what it is showing. */ 
  def changeUITextField(gui:T,shouldBe:String)
  /** XS sending a command to the GUI to change what an image is showing. The argument is a URL of an image. */ 
  def changeUIImageField(gui:T,shouldBe:String)
  /** XS sending a command to the GUI to change what it is showing. */ 
  def changeUIBooleanField(gui:T,shouldBe:Boolean)
  /** XS sending a command to the GUI to change the whole contents of the table */
  def changeUIWholeTable(gui:T,shouldBe:IndexedSeq[IndexedSeq[String]])
  /** XS sending a command to the GUI to change a particular line of the table */
  def changeUISingleLineTable(gui:T,index:Int,shouldBe:IndexedSeq[String])
  /** XS sending a command to the GUI to change whether a field is visible. */
  def changeUIvisibility(gui:T,visible:Boolean)
  /** XS sending a command to the GUI to change whether a field is enabled. */ 
  def changeUIenabledness(gui:T,enabled:Boolean)
  /** XS sending a command to the GUI to change what a pseudo-field is showing */
  def changeUIShowText(gui:T,shouldBe:RichLabel)
  /** XS sending a command to the GUI to change what an edit command field should look like */
  def changeUIShowCommands(gui:T,shouldBe:List[EditCommandDescriptionMadeConcrete])
  /** XS sending a command to the GUI to change what text a label is */
  def changeUILabelText(gui:T,shouldBe:RichLabel)
  /** XS sending a command to the GUI to change what goes in a tooltip */
  def changeUITooltip(gui:T,tooltip:Option[RichLabel])
  /** XS sending a command to the GUI to change what icon a label has */
  def changeUILabelIcon(gui:T,icon:Option[Icon])
  /** XS sending a command to the GUI to change the errors shown for a field */
  def changeErrors(gui:T,errors:List[ResolvedXSError])
  /** XS sending a command to the GUI to change the errors shown for a field in a table. */
  def changeGridTooltip(gui:T,row:Int,col:Int,colfield:GeneralizedField,tooltip:Option[RichLabel])
  /** XS sending a command to the GUI to change the errors shown for a field in a table. */
  def changeGridErrors(gui:T,row:Int,col:Int,colfield:GeneralizedField,errors:List[ResolvedXSError])
  /** XS sending a command to the GUI to change the fact that the contents are illegal - eg a blank string box when a number is needed. This should do something like set a red background. */
  def setUIFieldIllegalContents(gui:T,isIllegal:Boolean)
  /** XS sending a command to the GUI to change the set of entries in a table that are illegal - eg a blank string box when a number is needed. This should do something like set a red background. The mey of the illegalEntries field is the row number; the contents are the column numbers. */
  def setUITableEntriesIllegalContents(gui:T,illegalEntries:Map[Int,List[Int]])
  /** XS sending a command to the GUI to redraw a custom control */
  
  //def changeUIShowCustom[S](gui:T,custom:CustomComponent[S],shouldBe:S,old:S)
  
  //
  // Clipboard management code - can (and should!) be overridden to provide non-local clipboard
  //
  
  private var localClipboard : Option[XSClipBoard] = None 
    
  def getClipboard(param:XSClipboardRequest) : Future[XSClipBoard] = future { localClipboard.get }(executionContext)
  def setClipboard(data:XSClipBoard) {
    localClipboard=Some(data)
  }


  
  //
  // Utility functions
  //
  private def makeNewGUI(tree:XSTreeNode,pane:DetailsPaneFields) : UIFields = {
    val creator = newCreator()    
    makeNewGUI(tree,pane,creator,NoMap)
  }
  
  private def makeNewGUI(tree:XSTreeNode,pane:DetailsPaneFields,creator:GUICreator[T],submap:SubNodeMap) : UIFields = {
    val buffer = new ListBuffer[UIField]
    def getState(field:DetailsPaneField) = {
        val enabled = field.shouldBeEnabled(tree)
        val visible = field.shouldBeVisible(tree)
        val icon = tree.specialIconForField(field.name)
        val canHaveSpecialIcon = tree.info.dependencyInjectionInfo.fieldIconProvider.contains(field.name)
        val label = tree.specialLabelForField(field.name,locale)
        val tooltip = tree.tooltipForField(field.name,locale)
        val canHaveSpecialLabel = tree.info.dependencyInjectionInfo.fieldLabelProvider.contains(field.name)
        val canHaveTooltip = tree.info.dependencyInjectionInfo.fieldTooltipProvider.contains(field.name)
        new CurrentFieldState(enabled,visible,icon,label,tooltip,canHaveSpecialIcon,canHaveSpecialLabel,canHaveTooltip)              
    }
    val headingState = getState(pane.field)
    val headingGUI = creator.startForm(pane.field,headingState,tree.info.htmlTemplate)
    buffer+=new UIFieldHeading(pane.field,headingGUI,headingState)
    for (section<-pane.sections) {
      val sectionState = getState(section.field)
      val sectionGUI = creator.startSection(section.field,sectionState)
      buffer+=new UIFieldHeading(section.field,sectionGUI,sectionState)
      for (field<-section.fields) {
        val state = getState(field)
        field match {
          case f:DetailsPaneFieldAction =>  
            val gui = creator.createAction(f,state)
            buffer+=new UIFieldAction(f,gui,state,creator.createProgressMonitor(gui),creator.getExecutionContext)
          case f:DetailsPaneFieldEditCommands =>  
            val initialValue = f.get(tree,locale)
            val gui = creator.createEditCommands(f,state,initialValue)
            buffer+=new UIFieldEditCommands(f,gui,state,initialValue,locale)
          case f:DetailsPaneFieldText =>
            val initialValue = f.get(tree)
            val gui = creator.createTextField(f, state, initialValue)
            buffer+=new UIFieldText(f,gui,state, initialValue,f.choices)
          case f:DetailsPaneFieldBoolean =>
            val initialValue = f.get(tree)
            val gui = creator.createBooleanField(f, state, initialValue)
            buffer+=new UIFieldBoolean(f,gui,state, initialValue)            
          case f:DetailsPaneFieldShowText =>
            val initialValue = f.get(tree,locale)
            val gui = creator.createShowTextField(f,state, initialValue)
            buffer+=new UIFieldShowText(f,gui,state, initialValue,locale)
          case f:DetailsPaneFieldCustom =>
            def proc[S](custom:CustomComponent[S,T]) {
                val initialValue = custom.state(tree,locale)
                val gui = creator.createCustom(f,custom,state, initialValue)
                val work = custom.getWork(this, gui, initialValue)
                buffer+=new UIFieldCustom(f,custom,gui,state, work,locale)              
            }
            for (custom<-getCustom(f)) proc(custom)
          case f:DetailsPaneFieldImage =>
            val initialValue = f.get(tree)
            val gui = creator.createImageField(f,state, initialValue)
            buffer+=new UIFieldImage(f,gui,state, initialValue) 
          case f:DetailsPaneFieldTable =>
            val initialValue = f.get(tree)._2
            val gui = creator.createTableField(f,state, initialValue)
            buffer+=new UIFieldTable(f,gui,state, initialValue)             
          case f:DetailsPaneFieldInline =>
            //val initialValue = f.get(tree) // doesn't create the subfields - they will return later.
            val (gui,newCreator) = creator.createInlineField(f,state)
            buffer+=new UIFieldInline(f,gui,state,newCreator,IndexedSeq.empty,Nil,submap)             
          case f:DetailsPaneFieldSection => throw new IllegalArgumentException
        }  
      }
      creator.endSection(section.field,sectionGUI,sectionState)
    }
    val wholegui = creator.endForm()
    new UIFields(buffer.toList,submap,wholegui)
  }
  
  def refresh() {
    synchronized {
      for (uifields<-currentUIElements; node<-currentlyShowing; uifield<-uifields.elems) uifield.refresh(node)
      flushClientCommands()
    }
  }

  private def containsInlineElements = currentUIElements.map{_.containsInlineElements}.getOrElse(false)
  
  def refresh(changes:TreeChange) {
    val cs = currentlyShowing.getOrElse(null)
    //println("XSDetailsPane.refresh "+cs)
    if (changes.contains(cs) || (cs!=null && containsInlineElements && changes.elementsIncludingRecursive.exists{_.parent.isDescendentOf(cs)})) refresh() // It would be possible to optimize the second case.
    else if (cs!=null) for (c<-changes.elementsIncludingRecursive) if (c.parent.parent==cs) c.parent.getTableLine match {
      case None =>
        //println("Found a child of the thing being displayed that is not a table line")
      case Some((field,linenumber)) => // We have a change for that particular line number for that particular field.
        //println("Found a child that is a table line")
        for (uifields<-currentUIElements;uifield<-uifields.elems) uifield match {
          case t:UIFieldTable if t.field.field==field && c.parent.parent==cs => 
           // println("Found table "+t.field.name)
            t.refresh(c.parent,linenumber)
            flushClientCommands()
          case f =>
           // println("Not "+f.field+" called "+f.field.name+" looking for "+field+" called "+field.name)
        }
    }
  }

  def userHasAbandonedNoncanonicalEdits() {
    synchronized {
      for (uifields<-currentUIElements; node<-currentlyShowing; uifield<-uifields.elems) uifield.userHasAbandonedNoncanonicalEdits()
    }    
  }
  /** The fields currently being shown */
  class UIFields(val elems:List[UIField],submap:SubNodeMap,val wholegui:T) {
    lazy val simplemap : Map[T,UIField] = Map.empty++(for (e<-elems) yield e.gui->e)
    lazy val inlines : List[UIFieldInline] = elems.collect{case f:UIFieldInline => f}
    lazy val containsInlineElements : Boolean = elems.exists(_.isInstanceOf[UIFieldInline])
    override def toString : String = elems.filter{_.currently.visible}.mkString("\n")
    def get(gui:T) : Option[UIFieldAndSubNodeMap] = {
      simplemap.get(gui) match {
        case Some(n) => Some(new UIFieldAndSubNodeMap(n,submap))
        case None => inlines.flatMap{_.getField(gui)}.headOption
      }
    }
    def updateSubmap(newsubmap:SubNodeMap) : UIFields = {
      if (submap==newsubmap) this
      else new UIFields(elems,newsubmap,wholegui)
    }
  }

  def uiField(gui:T) : Option[UIFieldAndSubNodeMap] = currentUIElements.flatMap{_.get(gui)}
  

  sealed abstract class SubNodeMap {
    def map(node:XSTreeNode) : Option[XSTreeNode]
  }
  object NoMap extends SubNodeMap {
    override def map(node:XSTreeNode) = Some(node)
  }
  /** Get the count'th element of field of the base */
  final class SimpleSubmap(val base:SubNodeMap,val field:XSFieldInfo,val count:Int) extends SubNodeMap {
    override def map(node:XSTreeNode) = base.map(node).flatMap{n=>
      val children = n.tableAndInlineChildren(field)
      if (children.length>count) Some(children(count)) else None
    }
    override def equals(o:Any) = o match {
      case other:SimpleSubmap => other.base==base && other.field==field && other.count==count
      case _ => false
    }
  }
  class UIFieldAndSubNodeMap(val field:UIField,val submap:SubNodeMap)
  
  sealed abstract class UIField  {
    def field:DetailsPaneField
    def gui:T
    def currently:CurrentFieldState
    def humanEditedTrimInfo:Array[Option[TrimInfo]]
    
    var currentlyShowingErrors : List[ResolvedXSError] = Nil

    /** 
     * Sometimes a field may have a value shown that is different from the actual value in the data structure.
     * This would be, for instance, if the field were an integer, and the user had backspaced it in preparation
     * to type a new number. We should leave what was there rather than replace it by zero or something. On the
     * other hand, we don't want to keep it forever (for instance, change the object currently being viewed). When
     * we no longer want to keep it, call this function.
     */
    def userHasAbandonedNoncanonicalEdits() {}
    
    /** Update the GUI with the contents of the object. */
    def refresh(node:XSTreeNode) : Unit
    
    def busy : Boolean = false
    
    def refreshCurrently(node:XSTreeNode) {
      // println("refreshCurrently() for "+field.name)
      val shouldBeEnabled = field.shouldBeEnabled(node) && !busy
      if (shouldBeEnabled!=currently.enabled) {
        currently.enabled=shouldBeEnabled
        changeUIenabledness(gui,shouldBeEnabled)
      }    
      val shouldBeVisible = field.shouldBeVisible(node)
      if (shouldBeVisible!=currently.visible) {
        currently.visible=shouldBeVisible
        changeUIvisibility(gui,shouldBeVisible)
      }    
      if (currently.canHaveSpecialIcon) {
        val specialIcon = node.specialIconForField(field.name)
        // println("Special icon = "+specialIcon)
        if (specialIcon!=currently.specialIcon) {
          currently.specialIcon=specialIcon
          changeUILabelIcon(gui,specialIcon.orElse(field.icon))
        }
      }
      if (currently.canHaveSpecialLabel) {
        val specialLabel = node.specialLabelForField(field.name,locale)
        // println("Special label = "+specialLabel)
        if (specialLabel!=currently.specialLabel) {
          currently.specialLabel=specialLabel
          changeUILabelText(gui,specialLabel.getOrElse(RichLabel(field.label)))
        }
      }
      if (currently.canHaveTooltip) {
        val tooltip = node.tooltipForField(field.name,locale)
        if (tooltip!=currently.tooltip) {
          currently.tooltip=tooltip
          changeUITooltip(gui,tooltip)
        }
      }
      if (field.couldContainErrorIcon) {
        val errors = node.errors(field.name,locale,humanEditedTrimInfo)
        if (errors!=currentlyShowingErrors) {
          currentlyShowingErrors=errors
          changeShownErrors(errors)
        }
      }
    }

    def changeShownErrors(errors:List[ResolvedXSError]) { changeErrors(gui,errors) }
  }

  class UIFieldHeading(val field:DetailsPaneField,val gui:T,var currently:CurrentFieldState) extends UIField {
    def humanEditedTrimInfo:Array[Option[TrimInfo]] = TrimInfo.empty
    def refresh(node:XSTreeNode) { refreshCurrently(node) }
    override def toString = field.toString
  }
  
  abstract class UIFieldIsStringOrCollectionOfStrings extends UIField {
    def currentlyShowing:String
    var currentlyIsIllegalValue = false
    var currentlyShowingIsIllegalValue = false
    /** This is the canonical representation of the value set by the string the user has entered, or None if this is not recently user edited. */
    protected[this] var currentStringSetByUserCanonicalRepresentation : Option[String] = None
    def field : DetailsPaneFieldLabeled with DetailsPaneFieldBasedOnSimpleField
    override def userHasAbandonedNoncanonicalEdits() {
      //println("userHasAbandonedNoncanonicalEdits()")
      synchronized {
        currentStringSetByUserCanonicalRepresentation = None
        currentlyIsIllegalValue = false    
      }
    }
    override def humanEditedTrimInfo:Array[Option[TrimInfo]] = {
      synchronized {
        if (currentStringSetByUserCanonicalRepresentation.isDefined && !currentlyIsIllegalValue) {
          val res:Array[Option[TrimInfo]] = if (field.field.isCollectionOrArrayButNotOption) { if (currentlyShowing.isEmpty()) Array() else CollectionStringUtil.separateSemicolonListEscaped(currentlyShowing,false).map{s=>Some(TrimInfo(s))}}
          else Array(Some(TrimInfo(currentlyShowing)))
         // println(res.mkString(";"))
          res
        } else TrimInfo.empty
      }
    }
    def refreshIllegal() {
        if (currentlyIsIllegalValue!=currentlyShowingIsIllegalValue) {
          setUIFieldIllegalContents(gui,currentlyIsIllegalValue)
          currentlyShowingIsIllegalValue=currentlyIsIllegalValue
        }
    }
    
    override def toString = field.label+":"+currentlyShowing
    
  }
  
  class InlineBlock(val btype:SerializableTypeInfo[_],val fields:UIFields) {
    def destroy() { 
      dispose(fields)
      remove(fields.wholegui)
    }
    private var lastRefreshedNode:XSTreeNode=null
    def removeNonCanonicalsIfNotLatest(node:XSTreeNode) {
      if (lastRefreshedNode!=node) {
        for (f<-fields.elems) f.userHasAbandonedNoncanonicalEdits()
      }
    }
    def refresh(node:XSTreeNode) {
      lastRefreshedNode=node
      for (f<-fields.elems) f.refresh(node)
    }
  }
  
  class UIFieldInline(val field:DetailsPaneFieldInline,val gui:T,var currently:CurrentFieldState,val creator:GUICreator[T],var currentTypes:IndexedSeq[SerializableTypeInfo[_]],var currentBlocks:List[InlineBlock],baseSubmap:SubNodeMap) extends UIField { 
    def humanEditedTrimInfo:Array[Option[TrimInfo]] = TrimInfo.empty
    def refresh(node:XSTreeNode) { synchronized { 
      val subfields : IndexedSeq[XSTreeNode] = node.tableAndInlineChildren(field.field)
      val shouldBeTypes : IndexedSeq[SerializableTypeInfo[_]] = subfields.map{_.info}
      if (currentTypes!=shouldBeTypes) {
        var left = currentBlocks
        var newBlocks = new ListBuffer[InlineBlock]
        var count=0
        for (sf<-subfields) {
          val t = sf.info
          val submap:SubNodeMap = new SimpleSubmap(baseSubmap,field.field,count)
          while (left.nonEmpty && left.head.btype!=t) { left.head.destroy(); left=left.tail }
          if (left.nonEmpty) {
            val old = left.head
            old.removeNonCanonicalsIfNotLatest(sf)
            newBlocks+=new InlineBlock(t,old.fields.updateSubmap(submap)) 
            left=left.tail
          } else {
            val pane:DetailsPaneFields = t.getPane(locale,sf.mayDelete)
            val fields = makeNewGUI(sf,pane,creator,submap:SubNodeMap) 
            newBlocks+=new InlineBlock(t,fields)
          }
          count+=1
        }
        for (old<-left) old.destroy()
        currentTypes=shouldBeTypes
        currentBlocks = newBlocks.toList
      }
      for ((b,n)<-currentBlocks.zip(subfields)) b.refresh(n)
      refreshCurrently(node) 
    }}
    def dispose() {
      synchronized {
        for (b<-currentBlocks) basePane.dispose(b.fields)
      }
    }
    override def toString = field.toString
    def getField(gui:T) : Option[UIFieldAndSubNodeMap] = currentBlocks.flatMap{_.fields.get(gui)}.headOption
  }
  

  
  class UIFieldText(val field:DetailsPaneFieldText,val gui:T,var currently:CurrentFieldState,var currentlyShowing:String,val choices:Option[LocalizedTextChoices]) extends UIFieldIsStringOrCollectionOfStrings {
    /** 
     * Sometimes the user may set a field to some value that is slightly different to the canonical one - for instance, leading zeros on an integer. 
     * In that case we want to leave it until it is changed for some other reason. So the "currentlyShowing" is what text is currently on the client,
     * and currentStringSetByUser is, if not None, the canonical representation of what is actually showing. If what is currently showing is an
     * illegal value, then it stores the last legal thing that was there.
     */
    def changedByGUI(node:XSTreeNode,s:String) { 
      //println("changedByGUI1("+s+") currentStringSetByUser="+currentStringSetByUser)
      synchronized {
        currentlyShowing=s
        currentlyIsIllegalValue = !field.set(node,s,Some( ()=>{currentStringSetByUserCanonicalRepresentation=Some(field.get(node))}))
        if (currentlyIsIllegalValue!=currentlyShowingIsIllegalValue) { refresh(node) ; flushClientCommands()}
      }
      //println("changedByGUI3("+s+") currentStringSetByUser="+currentStringSetByUser)
    }
    def refresh(node:XSTreeNode) {
      //println("Refreshing node "+node.uid)
      synchronized {
        val shouldBe = field.get(node)
        //println("refresh shouldBe="+shouldBe+" showing="+currentlyShowing+" currentStringSetByUserCanonicalRepresentation="+currentStringSetByUserCanonicalRepresentation)
        if (shouldBe!=currentlyShowing && (currentStringSetByUserCanonicalRepresentation.isEmpty || currentStringSetByUserCanonicalRepresentation.get!=shouldBe)) { 
          currentlyShowing=shouldBe
          currentStringSetByUserCanonicalRepresentation=None
          currentlyIsIllegalValue = false
          //println("** Change field "+gui+" to "+shouldBe)
          changeUITextField(gui,shouldBe)
        }  
        refreshIllegal()
      }
      refreshCurrently(node)
    }
  }

  class TableNoncanonicalEntry(val row:XSTreeNode,val col:Int,val humanEditedValue:String,val lastCanonicalValue:String,val isIllegalValue:Boolean) {
    override def toString = "human typed "+humanEditedValue+" instead of "+lastCanonicalValue+" in column "+col+(if (isIllegalValue) " illegally" else "")
  }
  
  class UIFieldTable(val field:DetailsPaneFieldTable,val gui:T,var currently:CurrentFieldState,var currentlyShowing:IndexedSeq[IndexedSeq[String]]) extends UIField {
    
    def changedByGUI(node:XSTreeNode,s:String) { 
    }
    override def humanEditedTrimInfo:Array[Option[TrimInfo]] = TrimInfo.empty
    override def userHasAbandonedNoncanonicalEdits() {
      synchronized {
      }
    }
    def printNoncanonicals(msg:String) {
      if (false) {
        println("Non-canonicals "+msg)
        for ((row,edits)<-humanEdits) {
          println("Row "+row)
          for (e<-edits) println(e.toString)
        }
      }
    }
    private def modifyElem(baseelem:AnyRef,colfield:XSFieldInfo,newText:String,node:XSTreeNode,colno:Int) : (AnyRef,Option[AnyRef => () => Unit]) = {
      val parsed = colfield.parseStringPossiblyMultipleSafe(newText)
      val (newobj,isError) = parsed match {
        case Success(newParsedValue) => 
          val newelem = field.field.xsinfo.get.setFieldAnyRef(baseelem,colfield,newParsedValue) 
          (newelem,false)
        case Failure(e) => 
          (baseelem,true)
      } 
      val noncanonfn : Option[AnyRef => (() => Unit)] = {
          val canonical = colfield.getFieldAsString(newobj)
          if (isError || canonical!=newText) Some({finalobj:AnyRef => {() => {
            for (row<-node.tableAndInlineChildren(field.field).find{_.getObject eq finalobj}) 
              addNoncanonicalEntry(new TableNoncanonicalEntry(row,colno,newText,canonical,isError))
          }}}) else None
      }
      (newobj,noncanonfn)
    }
    def addedNewRow(node:XSTreeNode,columnName:String,newText:String) {
      val (colfield,colno) = field.columnExtractors.xsFieldNamed(columnName)
      val newRow = for (i<-0 until field.columnExtractors.length) yield if (i==colno) newText else ""
      val baseelem = field.field.newSingleElement()
      val (newobj,noncanonfn) = modifyElem(baseelem:AnyRef,colfield:XSFieldInfo,newText:String,node:XSTreeNode,colno:Int)
      synchronized {
         currentlyShowing=currentlyShowing :+ newRow
//         println("Adding "+newobj+" to field "+colfield.name)
         node.xsedit.addField(node,None,field.field,newobj,noncanonfn.map{_(newobj)},"add row")
      }
//      println("Added new row for column "+columnName+" value "+newValue)
    }
    def addNoncanonicalEntry(entry:TableNoncanonicalEntry) {
      removeNoncanonicalEntry(entry.row,entry.col)
      humanEdits+=entry.row->(entry::(humanEdits.get(entry.row).getOrElse(Nil)))
    }
    def removeNoncanonicalEntry(row:XSTreeNode,colno:Int) {
      for (edits<-humanEdits.get(row)) humanEdits+=row->edits.filter{_.col!=colno}
    }
    def changedElement(node:XSTreeNode,rowNumber:Int,columnName:String,newValue:String) {
      val (colfield,colno) = field.columnExtractors.xsFieldNamed(columnName)
      synchronized {
        val rows = node.tableAndInlineChildren(field.field)
        if (rowNumber==currentlyShowing.length && rowNumber==rows.length) { // adding a new row
          // TODO fix lack of syntax checking on starting to add a new row in a grid.
          // the line below would make the XS backend be aware of a new row being added. However, this mixes very badly with the SlickGrid row
          // update model, which is unfortunate. Not having it means that the new field is not checked for errors etc. until it is actually comitted.
          //addedNewRow(node,columnName,newValue);
        }
        if (rowNumber<currentlyShowing.length && rowNumber<rows.length) {
          currentlyShowing=currentlyShowing.updated(rowNumber,currentlyShowing(rowNumber).updated(colno,newValue))
          val row = rows(rowNumber)
          colfield.parseStringPossiblyMultipleSafe(newValue) match {
                case Success(newParsedValue) =>
                  //println("Setting field "+colfield+" to "+newParsedValue+" for row "+row)
                  row.xsedit.setField(row,colfield,newParsedValue,Some( ()=>{
                    // see if should be non-canonical.
                    val canonical = colfield.getFieldAsString(row.getObject)
                    if (canonical==newValue) {
                      removeNoncanonicalEntry(row,colno)
                    } else {
                      addNoncanonicalEntry(new TableNoncanonicalEntry(row,colno,newValue,canonical,false))
                    }
                  })) 
                case Failure(e) => // mark as error 
                  val canonical = colfield.getFieldAsString(row.getObject)
                  addNoncanonicalEntry(new TableNoncanonicalEntry(row,colno,newValue,canonical,true))
                  refresh(node)
                  flushClientCommands()
          }
          
        }       
      }
      //printNoncanonicals("Edited")
      //println("Edited row "+rowNumber+" column "+columnName+" value "+newValue)
      
    }
    def pasteTable(node:XSTreeNode,rowNumber:Int,columnName:String,table:Array[Array[String]]) {
       val (_,colno) = field.columnExtractors.xsFieldNamed(columnName)
       //println("Paste table ("+rowNumber+","+colno+")\n"+table.map{_.mkString("\t")}.mkString("\n"))
       synchronized {
        val rows = node.tableAndInlineChildren(field.field)
        if (rowNumber<currentlyShowing.length && rowNumber<rows.length) {
          currentlyShowing=currentlyShowing.updated(rowNumber,currentlyShowing(rowNumber).updated(colno,""))
          val noncanonfns = new ListBuffer[()=>Unit]
          val newrows = new ArrayBuffer[AnyRef]
          for (i<-0 until rowNumber) newrows+=rows(i).getObject
          def modifyObj(pastedLine:Array[String],obj:AnyRef) = {
            var modobj = obj
            val partialnoncanonfns = new ListBuffer[AnyRef => () => Unit] // need to apply the final object after all column mods to this
            for (pastedColNum<-0 until pastedLine.length) {
              val workingColNum = colno+pastedColNum
              if (workingColNum<field.columnExtractors.fields.length) field.columnExtractors.fields(workingColNum) match {
                case colfield:XSFieldInfo =>       
                  val (newobj,noncanonfn) = modifyElem(modobj:AnyRef,colfield:XSFieldInfo,pastedLine(pastedColNum):String,node:XSTreeNode,workingColNum:Int)
                  modobj = newobj
                  for (f<-noncanonfn) partialnoncanonfns+=f
                case _ => // field is display only
              }
            }
            for (f<-partialnoncanonfns) noncanonfns+=f(modobj)
            modobj
          }
          for (pastedRowNum<-0 until table.length) {
            val workingRowNum=rowNumber+pastedRowNum
            val oldobj = if (workingRowNum<rows.length) rows(workingRowNum).getObject else field.field.newSingleElement()
            newrows+=modifyObj(table(pastedRowNum),oldobj)
          }
          for (i<-rowNumber+table.length until rows.length) newrows+=rows(i).getObject
          val noncanonfn = if (noncanonfns.isEmpty) None else Some(() => {for (f<-noncanonfns) f()})
          val newnodeobj = node.info.setFieldAnyRef(node.getObject,field.field,field.field.collectionOfBuffer(newrows.toIndexedSeq)) 
          node.xsedit.changeNode(node,newnodeobj,noncanonfn,"Paste table")          
        }       
      }

    }

    var humanEdits : Map[XSTreeNode,List[TableNoncanonicalEntry]] = Map.empty
    var currentlyShowingIllegalEntries : Map[Int,List[Int]] = Map.empty
    var currentlyShowingCellErrors : Map[(Int,Int),List[ResolvedXSError]] = Map.empty // first index is row, second is col
    var currentlyShowingTooltips : Map[(Int,Int),RichLabel] = Map.empty // first index is row, second is col
    
    def applyNonCanonicalEditingToLine(row:XSTreeNode,canonical:IndexedSeq[String]) : (IndexedSeq[String],Option[(XSTreeNode,List[TableNoncanonicalEntry])])= {
          var line = canonical
          val cleanedEdit = for (edits<-humanEdits.get(row)) yield {
              val cleanedEdits = new ListBuffer[TableNoncanonicalEntry] // garbage collection.
              for (e<-edits) {
                val is = line(e.col)
                if (is == e.lastCanonicalValue) {
                  cleanedEdits+=e
                  line = line.updated(e.col,e.humanEditedValue)
                }
              }
              row->cleanedEdits.toList
          }
          (line,cleanedEdit)      
    }
    
    def refreshErrors(line:XSTreeNode,linenumber:Int) {
     synchronized {
      for (colnumber<-0 until field.columnExtractors.fields.length) {
        val colfield = field.columnExtractors.fields(colnumber)
        val couldContainErrorIcon = field.field.xsinfo.get.dependencyInjectionInfo.fieldsThatCouldHaveErrors.contains(colfield.name)
        if (couldContainErrorIcon) {
           val errors = line.errors(colfield.name,locale,humanEditedTrimInfo)
           val key = (linenumber,colnumber)
           val oldErrors = currentlyShowingCellErrors.get(key).getOrElse(Nil)
           if (errors!=oldErrors) {
             //println("Found new errors "+errors.mkString(";")+" != old errors "+oldErrors.mkString(";"))
             if (errors.isEmpty) currentlyShowingCellErrors-=key
             else currentlyShowingCellErrors+=key->errors
             changeGridErrors(gui,linenumber,colnumber,colfield,errors)
           }
        }
        val couldContainTooltip = field.field.xsinfo.get.dependencyInjectionInfo.fieldTooltipProvider.contains(colfield.name)
        if (couldContainTooltip) {
           val tooltip : Option[RichLabel] = line.tooltipForField(colfield.name,locale)
           val key = (linenumber,colnumber)
           val oldTooltip = currentlyShowingTooltips.get(key)
           if (tooltip!=oldTooltip) {
             //println("Found new errors "+errors.mkString(";")+" != old errors "+oldErrors.mkString(";"))
             if (tooltip.isEmpty) currentlyShowingTooltips-=key
             else currentlyShowingTooltips+=key->tooltip.get
             changeGridTooltip(gui,linenumber,colnumber,colfield,tooltip)
           }
        }
      }
     }
    }
    def refresh(node:XSTreeNode) {
      //println("Refreshing node "+node.uid)
      synchronized {
        printNoncanonicals("before refresh")
        val (rows,shouldBeBase) = field.get(node)
        var cleanedHumanEdits : Map[XSTreeNode,List[TableNoncanonicalEntry]] = Map.empty
        var shouldBeIllegalEntries : Map[Int,List[Int]] = Map.empty // first entry is row number, second is column number
        val shouldBe = for (ind<-0 until rows.length) yield {
          val (fixedLine,cleanEdits) = applyNonCanonicalEditingToLine(rows(ind),shouldBeBase(ind))
          for (edits<-cleanEdits) { // over option
            cleanedHumanEdits+=edits 
            val errs = edits._2.filter{_.isIllegalValue}.map{_.col}
            if (!errs.isEmpty) shouldBeIllegalEntries+=ind->errs
          }
          fixedLine
        }
        humanEdits=cleanedHumanEdits // garbage collection. 
        printNoncanonicals("cleaned after refresh")
        if (shouldBe!=currentlyShowing) {
          val allbad = shouldBe.length!=currentlyShowing.length || { // only update lines that have changed, unless there are lots of them.
            val badlines = (0 until shouldBe.length).filter{ind=> currentlyShowing(ind)!=shouldBe(ind)}
            if (badlines.length==0) false
            else if (badlines.length<=1+shouldBe.length/2) { // update line by line
              for (linenumber<-badlines) changeUISingleLineTable(gui,linenumber, shouldBe(linenumber))
              false
            } else true
          }
          currentlyShowing=shouldBe;
          if (allbad) changeUIWholeTable(gui,shouldBe)      
        }
        //println("Refresh table shouldBeIllegalEntries="+shouldBeIllegalEntries.map{case (row,cols) => cols.map{""+row+","+_}.mkString("|")}.mkString("|"))
        if (currentlyShowingIllegalEntries!=shouldBeIllegalEntries) {
          setUITableEntriesIllegalContents(gui,shouldBeIllegalEntries)
          currentlyShowingIllegalEntries=shouldBeIllegalEntries
        }
        for (rownumber<-0 until rows.length) refreshErrors(rows(rownumber),rownumber)
      }
      refreshCurrently(node)
    }
    /** refresh a single line */
    def refresh(line:XSTreeNode,linenumber:Int) {
      //println("Refresh single line "+line+" #"+linenumber)
      //printNoncanonicals("refresh line")
      synchronized {
        val (shouldBe,cleanedEdits) = applyNonCanonicalEditingToLine(line,line.getTableFields(field.columnExtractors))
        cleanedEdits match {
          case Some(pair) => humanEdits+=pair
          case None => humanEdits-=line
        }
        if (currentlyShowing.length>linenumber && currentlyShowing(linenumber)!=shouldBe) {
          currentlyShowing=currentlyShowing.updated(linenumber,shouldBe)
          changeUISingleLineTable(gui,linenumber, shouldBe)
        }
        refreshErrors(line,linenumber)
      }
    }
    def uiDragGridRows(parent:XSTreeNode,rowsToBeMoved:Seq[Int],insertBefore:Int) {
      val allnodes = field.getNodes(parent)
      val validrows = rowsToBeMoved.sorted.collect{case r:Int if r>=0 && r<allnodes.length => allnodes(r)}
      parent.xsedit.dragData(parent,validrows,if (insertBefore<allnodes.length) Some(allnodes(insertBefore)) else None)
    }
    def uiTableContextMenu(parent:XSTreeNode,command:String,selectedRows:Seq[Int]) {
      val allnodes = field.getNodes(parent)
      val nodes = selectedRows.sorted.collect{case r:Int if r>=0 && r<allnodes.length => allnodes(r)}
        command match { // TODO make copy and paste tabular data
          case "copy" => setClipboard(xsedit.copyData(nodes))
          case "cut" => setClipboard(xsedit.copyData(nodes)); xsedit.deleteTreeNodes(nodes,"cut")
          case "paste" => //for (c<-clipboard) xsedit.pasteData(parent, c, nodes.headOption)
            getClipboard(XSClipboardRequest.xsSerializedData).onSuccess { case c =>  xsedit.pasteData(parent, c, nodes.headOption) }(executionContext)
          case "erase" => xsedit.deleteTreeNodes(nodes,"erase")
          case "insert" => 
            val newRow = for (i<-0 until field.columnExtractors.length) yield ""
            val baseelem = field.field.newSingleElement()
            parent.xsedit.addField(parent,nodes.headOption,field.field,baseelem,None,"insert row")

        }
    }
    override def toString = field.label+":"+currentlyShowing
  }

  class UIFieldImage(val field:DetailsPaneFieldImage,val gui:T,var currently:CurrentFieldState,var currentlyShowing:String) extends UIFieldIsStringOrCollectionOfStrings {
    def changedByGUI(node:XSTreeNode,s:String) { 
      currentlyShowing=s
      //println("Changing image to "+s)
      currentlyIsIllegalValue = !field.set(xsedit,node,s)
      if (currentlyIsIllegalValue!=currentlyShowingIsIllegalValue) { refresh(node) ; flushClientCommands()}
    }
    def refresh(node:XSTreeNode) {
      //println("Refreshing node "+node.uid)
      synchronized {
        val shouldBe = field.get(node)
        if (shouldBe!=currentlyShowing) { 
          currentlyShowing=shouldBe;
          changeUIImageField(gui,shouldBe)
        }
        refreshIllegal()
      }
      refreshCurrently(node)
    }
    override def toString = field.label+":"+currentlyShowing
  }


  class UIFieldBoolean(val field:DetailsPaneFieldBoolean,val gui:T,var currently:CurrentFieldState,var currentlyShowing:Boolean) extends UIField {
    def humanEditedTrimInfo:Array[Option[TrimInfo]] = TrimInfo.empty
    def changedByGUI(node:XSTreeNode,b:Boolean) { 
      currentlyShowing=b
      field.set(xsedit,node,b)
    }
    def refresh(node:XSTreeNode) {
      val shouldBe = field.get(node)
      //println("Refreshing node "+node.uid+" shouldBe="+shouldBe+" currentlyShowing="+currentlyShowing)
      if (shouldBe!=currentlyShowing) { 
        currentlyShowing=shouldBe;
        changeUIBooleanField(gui,shouldBe)
      }
      refreshCurrently(node)
    }
    override def toString = field.label+":"+currentlyShowing
  }

  class UIFieldAction(val field:DetailsPaneFieldAction,val gui:T,var currently:CurrentFieldState,getMonitor:()=>ProgressMonitor,executionContext:ExecutionContext) extends UIField {
    def humanEditedTrimInfo:Array[Option[TrimInfo]] = TrimInfo.empty

    var inProgress : Option[ProgressMonitor] = None
    override def busy = inProgress.isDefined
    
    def cancel() {
      for (p<-inProgress) p.cancel()
    }
    private def getMonitorWork(node:XSTreeNode) = { synchronized {
      if (busy) throw new ActionBusy()
      val monitor = getMonitor()
      inProgress = Some(monitor)
      refreshCurrently(node)
      flushClientCommands()
      monitor
    }}
    
    def refresh(node:XSTreeNode) {
      refreshCurrently(node)
    }
    def go(node:XSTreeNode) {
      if (busy) return;
      val progressMonitorInfo = new ProgressMonitorInfo {
        def getMonitor() : ProgressMonitor = getMonitorWork(node)
        def releaseMonitor() { inProgress=None; refreshCurrently(node); flushClientCommands() }
      }
      //println("UIFieldAction.go")
      refreshCurrently(node)
      field.go(xsedit,node,progressMonitorInfo,executionContext) 
    }
    override def toString = field.label
  }
  
  class UIFieldShowText(val field:DetailsPaneFieldShowText,val gui:T,var currently:CurrentFieldState,var currentlyShowing:RichLabel,locale:Locale) extends UIField {
    def humanEditedTrimInfo:Array[Option[TrimInfo]] = TrimInfo.empty
    def refresh(node:XSTreeNode) {
      //println("Refreshing node "+node.uid)
      val shouldBe = field.get(node,locale)
      if (shouldBe!=currentlyShowing) { 
        currentlyShowing=shouldBe;
        changeUIShowText(gui,shouldBe)
      }
      refreshCurrently(node)
    }
    override def toString = field.label
  } 

  class UIFieldEditCommands(val field:DetailsPaneFieldEditCommands,val gui:T,var currently:CurrentFieldState,var currentlyShowing:List[EditCommandDescriptionMadeConcrete],locale:Locale) extends UIField {
    def humanEditedTrimInfo:Array[Option[TrimInfo]] = TrimInfo.empty
    def refresh(node:XSTreeNode) {
      //println("Refreshing node "+node.uid)
      val shouldBe = field.get(node,locale)
      if (shouldBe.length!=currentlyShowing.length || !shouldBe.zip(currentlyShowing).forall{case (s,c) => s.isSameUI(c)}) { // check look the same
        changeUIShowCommands(gui,shouldBe)
      }
      currentlyShowing=shouldBe; // update run commands. Above check only checks the GUI is the same.
      refreshCurrently(node)
    }
    def go(sequence:Int,node:XSTreeNode) {
      val cs = currentlyShowing
      if (cs.length>sequence) {
        val c = cs(sequence)
        val res = c.run()
        if (res!=null) node.xsedit.changeNode(node,res,None,c.undoDescription)
      }
    }
    override def toString = field.label
  } 

  class UIFieldCustom[S](val field:DetailsPaneFieldCustom,val custom:CustomComponent[S,T],val gui:T,var currently:CurrentFieldState,val work:CustomComponentWork[S],locale:Locale) extends UIField {
    def humanEditedTrimInfo:Array[Option[TrimInfo]] = TrimInfo.empty
    def refresh(node:XSTreeNode) {
      //println("Refreshing node "+node.uid)
      work.refresh(custom.state(node,locale))
      /*
      val shouldBe = custom.state(node,locale)
      if (shouldBe!=currentlyShowing) {
        val old = currentlyShowing
        currentlyShowing=shouldBe;
        changeUIShowCustom(gui,custom,shouldBe,old)
      }*/
      refreshCurrently(node)
    }/*
    def humanInitiatedAction(node:XSTreeNode,action:Any) {
      work.
      currentlyShowing = custom.resolveHumanEdited(node,action)
    }*/
    override def toString = field.label
    
    override def changeShownErrors(errors:List[ResolvedXSError]) { 
      val (generalErrors,customErrors) = errors.partition{_.customComponentInformation.isEmpty}
      changeErrors(gui,generalErrors) 
      work.changeErrors(customErrors)
    }

  } 

  
  
}

class CurrentFieldState(var enabled:Boolean,var visible:Boolean,var specialIcon:Option[Icon],var specialLabel:Option[RichLabel],var tooltip:Option[RichLabel],val canHaveSpecialIcon:Boolean,val canHaveSpecialLabel:Boolean,val canHaveTooltip:Boolean)



/**
 * Client specific code that creates a GUI. T is the class of identifiers for the GUI objects. (eg in AWT, it could be a Component, in Swing a JComponent, in HTML a text string. 
 */
abstract class GUICreator[T] {
  /** First thing ever called, with information about headings, icons, etc. for the form. */
  def startForm(section:DetailsPaneFieldSection,currently:CurrentFieldState,template:Option[Node]) : T
  /** Called at the start of each section */
  def startSection(section:DetailsPaneFieldSection,currently:CurrentFieldState) : T
  /** Called to create an action field (inside a section) */
  def createAction(field:DetailsPaneFieldAction,currently:CurrentFieldState) : T
  /** Called for actions to get code that will, on demand, create a progress monitor for said field */
  def createProgressMonitor(gui:T) : ()=>ProgressMonitor = () =>ProgressMonitor.dummy
  /** Execution context used for actions */
  def getExecutionContext : ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  /** Called to create a text field (inside a section) */
  def createTextField(field:DetailsPaneFieldText,currently:CurrentFieldState,initialValue:String) : T
  /** Called to create an edit commands field (inside a section) */
  def createEditCommands(field:DetailsPaneFieldEditCommands,currently:CurrentFieldState,initialValue:List[EditCommandDescriptionMadeConcrete]) : T
  /** Called to create a boolean field - typically a checkbox (inside a section) */
  def createBooleanField(field:DetailsPaneFieldBoolean,currently:CurrentFieldState,initialValue:Boolean) : T
  /** Called to create an image field (inside a section) */
  def createImageField(field:DetailsPaneFieldImage,currently:CurrentFieldState,initialValue:String) : T
  /** Called to create a table field (inside a section) */
  def createTableField(field:DetailsPaneFieldTable,currently:CurrentFieldState,initialValue:IndexedSeq[IndexedSeq[String]]) : T
  /** Called to create a table field (inside a section). Current values not included. */
  def createInlineField(field:DetailsPaneFieldInline,currently:CurrentFieldState) : (T,GUICreator[T])
  /** Called to create a just-display text field (inside a section) */
  def createShowTextField(field:DetailsPaneFieldShowText,currently:CurrentFieldState,initialValue:RichLabel) : T
  /** Called to create a custom field (inside a section) */
  def createCustom[S](field:DetailsPaneFieldCustom,custom:CustomComponent[S,T],currently:CurrentFieldState,initialValue:S) : T
  /** Called at the end of each section, with the id created in startSection */
  def endSection(section:DetailsPaneFieldSection,id:T,currently:CurrentFieldState)
  /** Last thing called when the form is finished */
  def endForm() : T
}
