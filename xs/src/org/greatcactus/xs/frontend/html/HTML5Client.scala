/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.frontend.html

import scala.collection.mutable.ListBuffer
import org.greatcactus.xs.frontend.XSEdit
import java.util.Locale
import org.greatcactus.xs.frontend.XSTreeNode
import org.greatcactus.xs.frontend.XSEditListener
import org.greatcactus.xs.frontend.TreeChange
import org.greatcactus.xs.api.icon.Icon
import scala.xml.NodeSeq
import org.greatcactus.xs.api.display.RichLabel
import org.greatcactus.xs.frontend.TreeNodeChange
import org.greatcactus.xs.frontend.XSToolBar
import org.greatcactus.xs.frontend.ToolbarStatusListener
import org.greatcactus.xs.frontend.StatusForToolbar
import org.greatcactus.xs.frontend.XSClipboardRequest
import scala.concurrent.ExecutionContext
import org.greatcactus.xs.frontend.XSClipBoard
import org.greatcactus.xs.api.serialization.StringMapSerialize
import org.greatcactus.xs.impl.QueueEmptyStatusListener
import org.greatcactus.xs.impl.QueueEmptyStatus
//import scala.concurrent.ExecutionContext.Implicits.global
/**
 * The controller for communication with an HTML client. The details of the transport are not
 * included here - it could be via HTTP or Websockets.
 */
class HTML5Client(val xsedit:XSEdit,val toolbar:Option[XSToolBar],val locale:Locale,val executionContext:ExecutionContext) {

  private[this] val cmdBuffer = new ClientMessageBuffer
  
  private var processMessages : List[PartialFunction[SimpleClientMessage,Unit]] = Nil
  def addMessageProcessor(p:PartialFunction[SimpleClientMessage,Unit]) = {
    synchronized {
      processMessages=p::processMessages
    }
  }
  def removeMessageProcessor(p:PartialFunction[SimpleClientMessage,Unit]) = {
    synchronized {
      processMessages=processMessages.filter{_ ne p}
    }
  }
  

  val session = new HTTPSession(new HTTPSessionWorker{
    override def receivedMessage(message:SimpleClientMessage) { process(message) }
    override def dispose() {
      xsedit.removeDetailsPane(detailsPane)
      xsedit.removeTreeListener(treeListener)
      xsedit.dependencyInjectionCleaningQueue.removeQueueEmptyListener(queueEmptyListener)
      xsedit.unregisterActiveEditor()
    }
    override def getDraggedElement(subid:String) : Option[XSClipBoard] = {
      treePane.ID.unapply(subid) match {
        case Some(node) => Some(xsedit.copyData(List(node.node)))
        case None => None // could conceivably get something from a table in the detailsPane.
      }
      
    }
  })
  val transport = new HTMLTransport {
    def sendMessageWork(message:ClientMessage) { session.addMessage(message) }
  }
  val detailsPane : HTML5DetailsPane = new HTML5DetailsPane(this)
  xsedit.addDetailsPane(detailsPane)
  
  val queueEmptyListener : QueueEmptyStatusListener = new QueueEmptyStatusListener {
    override def queueEmptyStatusChanged(status:QueueEmptyStatus) {
      session.addMessage(ClientMessage.workQueueStatus(status.isNowEmpty,status.sequenceNumber))
    }
  }
  xsedit.dependencyInjectionCleaningQueue.addQueueEmptyListener(queueEmptyListener)

    /** If true, then modify the URL to store the currently selected element via a query string ?selected=xxxx */
  var savePermalinkInURL = false

  val treeModel = new TreeModel[XSTreeNode] {
      def children(node:XSTreeNode) : Seq[XSTreeNode] = node.treeChildren
      def title(node:XSTreeNode) : RichLabel = node.label(locale)
      def icon(node:XSTreeNode) : Option[String] = for (icon<-node.icon; ri <-icon.getInstance(Icon.html5formats,Some(16))) yield ri.url
      def userToggledStatus(node:XSTreeNode,isNowOpen:Boolean) { xsedit.setOpen(node,isNowOpen)}
      def userSelected(node:XSTreeNode) { xsedit.changeCurrentlyEditing(node); }
      def uniqueID(node:XSTreeNode) : String = node.uid.toString
      def isOpen(node:XSTreeNode) : Boolean = node.isOpen
      def dragLocal(source:XSTreeNode,dest:XSTreeNode,isAbove:Boolean) {
        val above = isAbove && dest.parent!=null
        xsedit.dragData(if (above) dest.parent else dest,List(source),if (above) Some(dest) else None)
      } 
      def errorLevel(node:XSTreeNode) : Int = node.worstErrorLevel  
      def userContextMenu(command:String,nodes:Seq[XSTreeNode]) {
        command match {
          case "copy" => session.setClipboard(xsedit.copyData(nodes))
          case "cut" => session.setClipboard(xsedit.copyData(nodes)); xsedit.deleteTreeNodes(nodes,"cut")
          case "paste" => session.getClipboard(XSClipboardRequest.xsSerializedData,executionContext).onSuccess{ case c=> for (node<-nodes.headOption) xsedit.pasteData(node, c, None) }(executionContext)
          case "erase" => xsedit.deleteTreeNodes(nodes,"erase")
        }
      }
      def dragInFile(dest:XSTreeNode,isAbove:Boolean,contents:Array[Byte],filename:String,lastModified:Option[Long]) {}
      def nonLocalDrag(dest:XSTreeNode,isAbove:Boolean,clip:XSClipBoard) {
        val parent = if (isAbove && dest.parent!=null) dest.parent else dest
        xsedit.pasteData(parent, clip, if (isAbove) Some(dest) else None, "drag")
      }
  }
  val treePane : HTML5Tree[XSTreeNode] = new HTML5Tree(locale,transport,treeModel,xsedit.treeRoot,session.sessionPrefix,session.id,false,true) 
  addMessageProcessor(treePane.processMessages)
  var storeWholeEditedObjectInURL : Boolean = false
  var lastSentURL : String = null
  
  private val treeListener : XSEditListener = new XSEditListener() {
      def apply(changes:TreeChange) { 
        //println("Got tree changes")
        def process(e:TreeNodeChange) {
          if (!e.parent.isTableLine) treePane.refresh(e.parent)
          for (sub<-e.sub) process(sub)          
        }
        for (e<-changes.elements) process(e)
        if (storeWholeEditedObjectInURL) {
          val newurl = StringMapSerialize.toURL(xsedit.currentObject)
          if (lastSentURL!=newurl) {
            lastSentURL=newurl
            sendMessage(ClientMessage.changeURLQuery(lastSentURL))
          }
        }
        flushMessages()
      }
      def setCurrentlyEditing(node:Option[XSTreeNode]) { 
        treePane.setSelected(node,false)
        flushMessages()
      }
  }
  
  xsedit.addTreeListener(treeListener)
  
  var toolbarUndoCurrentValue : Option[String] = Some("")
  var toolbarRedoCurrentValue : Option[String] = Some("")
  var saveEnabledCurrentValue = true
  var revertEnabledCurrentValue = true
  var staleCurrentValue=false
  val toolbarIDprefix = session.sessionPrefix+"toolbar."
  

  val toolbarListener = new ToolbarStatusListener {
    override def apply(status:StatusForToolbar) {
      for (t<-toolbar) synchronized {
        if (t.useUndoRedo) {
          if (toolbarUndoCurrentValue!=status.undoDesc) {
            toolbarUndoCurrentValue=status.undoDesc
            sendMessage(ClientMessage.setToolbarStatus(toolbarIDprefix+"undo",status.undoDesc.isDefined,status.undoDesc match { case Some(s) if s!=null => "Undo "+s; case _ => "Undo" }))
          }
          if (toolbarRedoCurrentValue!=status.redoDesc) {
            toolbarRedoCurrentValue=status.redoDesc
            sendMessage(ClientMessage.setToolbarStatus(toolbarIDprefix+"redo",status.redoDesc.isDefined,status.redoDesc match { case Some(s) if s!=null => "Redo "+s; case _ => "Redo" }))
          }
        }
        if (saveEnabledCurrentValue!=status.dirty) {
          saveEnabledCurrentValue=status.dirty
          sendMessage(ClientMessage.setToolbarStatus(toolbarIDprefix+"save",status.dirty,"Save"))
        }
        
        if (revertEnabledCurrentValue!=status.dirty) {
          revertEnabledCurrentValue=status.dirty
          sendMessage(ClientMessage.setToolbarStatus(toolbarIDprefix+"revert",status.dirty,"Revert"))
        }
        if (staleCurrentValue!=status.stale) {
          staleCurrentValue=status.stale
          sendMessage(ClientMessage.setToolbarStatus(toolbarIDprefix+"stale",status.stale,if (status.stale) (<em>While you were editing, the file was changed elsewhere.</em>).toString else ""))
        }
      }
    }
  }
  xsedit.addToolbarStatusListener(toolbarListener)
  
  // FIXME add something that shuts down the client when the session expires to stop a memory leak. The client currently does a decent job of warning the server of a closed window, but crashes are still a possibility.
  
  private def process(message:SimpleClientMessage) {
    try {
      transport.startBuffering();
      val args = message.args
      // first see if a custom controller can deal with the message
      for (c<-detailsPane.customControllerProcessMessages) {
        //println("Got message "+message)
        if (c.isDefinedAt(message)) {
          //println("Sent to custom")
          c(message)
          return
        }
      }
      for (c<-processMessages) {
        //println("Got message "+message)
        if (c.isDefinedAt(message)) {
          //println("Sent to custom")
          c(message)
          return
        }
      }
      message.command match {
        case "Action" if args.length==3 =>
          //println("Got action command "+args(0))
          //println("for context "+args(1)+" expecting "+detailsPane.nodeIDCurrentlyBeingEdited)
          if (detailsPane.nodeIDCurrentlyBeingEdited==args(2)) detailsPane.uiActivated(args(0),args(1).toInt); 
        case "Change" if args.length==3||args.length==5 =>
          //println("Got change command "+args(0)+" "+args(1))
          if (detailsPane.nodeIDCurrentlyBeingEdited==args(2)) {
            val id = args(0)
            val newValue = args(1)
            val gridInd = id.lastIndexOf("_grid")
            if (args.length==5 && !args(3).isEmpty() && !args(4).isEmpty && gridInd!= -1) detailsPane.uiChangeGrid(id.substring(0,gridInd),args(3).toInt,args(4),newValue)
            else detailsPane.uiChangedTextField(id,newValue,true); 
          }
        case "ChangeCB" if args.length==3 =>
          //println("Got change command "+args(0)+" "+args(1))
          if (detailsPane.nodeIDCurrentlyBeingEdited==args(2)) detailsPane.uiChangedBooleanField(args(0),args(1).toBoolean); 
        case "KeyPress" if args.length==3 =>
          //println("Got keypress command "+args(0))
          if (detailsPane.nodeIDCurrentlyBeingEdited==args(2)) detailsPane.uiChangedTextField(args(0),args(1),false); 
        case "GridDnD" if args.length==4 =>
          if (detailsPane.nodeIDCurrentlyBeingEdited==args(3)) {
            detailsPane.uiDragGridRows(args(0),args(1).split(',').map{_.toInt},args(2).toInt)
          }
        case "TableContextMenu" if args.length==4 =>
          if (detailsPane.nodeIDCurrentlyBeingEdited==args(3)) {
            detailsPane.uiTableContextMenu(args(0),args(1),args(2).split(',').map{_.toInt})          
          }
        case "Edited" if args.length==2 => detailsPane.uiChangedTextField(args(0),args(1),true)
        case "PartialEdit" if args.length==2 => detailsPane.uiChangedTextField(args(0),args(1),false)
        case "NewRowOnGrid" if args.length==4 => 
          if (detailsPane.nodeIDCurrentlyBeingEdited==args(3)) detailsPane.uiNewRowOnGrid(args(0),args(1),args(2))
        case "ChangeGrid" if args.length==5 =>  
          if (detailsPane.nodeIDCurrentlyBeingEdited==args(4)) detailsPane.uiChangeGrid(args(0),args(1).toInt,args(2),args(3))
        case "PasteTable" =>
          val id = args(0)
          val gridInd = id.lastIndexOf("_grid")
          if (detailsPane.nodeIDCurrentlyBeingEdited==args(1) && gridInd!= -1) detailsPane.uiPasteGrid(id.substring(0,gridInd),args(2).toInt,args(3),ClientMessage.unmungePasteGrid(args))          
        case "Toolbar" if args.length==1 =>
          args(0) match {
            case "undo" => xsedit.undo()
            case "redo" => xsedit.redo()
            case "save" => for (t<-toolbar) { t.onSave(); xsedit.undoRedo.reset(xsedit.currentObject); xsedit.updateToolbar() }
            case "revert" => for (t<-toolbar) t.onRevert()
            case _ =>
          }
        case "CancelJob" if args.length==2 =>
          if (detailsPane.nodeIDCurrentlyBeingEdited==args(1)) detailsPane.uiCancelJob(args(0))
        case "InitiatePopup" if args.length==2 =>
          if (detailsPane.nodeIDCurrentlyBeingEdited==args(1)) detailsPane.uiInitiatePopup(args(0))
        case "ClosedPopup" if args.length==2 =>
          if (detailsPane.nodeIDCurrentlyBeingEdited==args(1)) detailsPane.uiClosedPopup(args(0))
        case "PopupSetField" if args.length==3 =>
          if (detailsPane.nodeIDCurrentlyBeingEdited==args(1)) detailsPane.uiPopupSetField(args(0),detailsPane.popupMungeOutput(args(2)))
        case _ => println("Received unanticipated command "+message.command+"("+args.mkString(",")+")")
      }
    } catch { 
      case e:Exception => e.printStackTrace() // don't crash any event thread, etc. 
    } finally { 
      transport.endBuffering() 
    }
  }
  
  private def messageOfBuffer(buffer:ListBuffer[ClientMessage]): Option[ClientMessage] = {
      val cmds = buffer.toList
      buffer.clear()
      cmds match {
        case Nil => None
        case h::Nil => Some(h)
        case l => Some(new MultipleClientMessage(l))
      }  
  }
  
  
  def sendMessage(msg:ClientMessage) { queueMessage(msg); flushMessages() }
  def queueMessage(msg:ClientMessage) { 
    cmdBuffer+=msg
   // println("Message "+msg)
  }
  def flushMessages() {
    //println("FlushMessages()")
    for (m<-cmdBuffer.get()) transport.sendMessage(m)
  }

  def toolbarHTML = toolbar match {
    case Some(t) =>
     def button(id:String,text:String) = {
       <button id={toolbarIDprefix+id} class={"xsToolbarButton"+id} onclick={session.sessionPrefix+"toolbar('"+id+"')"}>{text}</button>
     }
     <div class="xsToolbar">
      {if (t.useSave) button("save","Save") else NodeSeq.Empty}    
      {if (t.useRevert) button("revert","Revert") else NodeSeq.Empty}    
      {if (t.useUndoRedo) button("undo","Undo") else NodeSeq.Empty}    
      {if (t.useUndoRedo) button("redo","Redo") else NodeSeq.Empty}
      {for (id<-t.others) yield button(id,id) }
      <div id={toolbarIDprefix+"stale"}></div>
     </div>
    case None => NodeSeq.Empty
  } 

  def mainPanelHTML = 
    <div class="xsEdit">
       {treePane.baseHTML()}
       { detailsPane.baseHTML }
       { session.createSessionHTML }
    </div>
       
  def justDetailsPaneHTML : NodeSeq = detailsPane.baseHTML++session.createSessionHTML     
  def baseHTML = toolbarHTML++mainPanelHTML 
  
}

abstract class HTMLTransport {
  private[this] val buffer = new ClientMessageBuffer
  private[this] var ifZeroThenDontBuffer = 0
  def startBuffering() { synchronized { ifZeroThenDontBuffer+=1 }}
  def endBuffering() { 
    synchronized { 
      ifZeroThenDontBuffer-=1
      if (ifZeroThenDontBuffer==0) for (m<-buffer.get()) sendMessageWork(m)
    }
  }
  
  def sendMessage(message:ClientMessage) {
     synchronized { 
         if (ifZeroThenDontBuffer==0) sendMessageWork(message)
         else buffer+=message
     }
  }

  /** Should be implemented by the client */
  def sendMessageWork(message:ClientMessage)
}