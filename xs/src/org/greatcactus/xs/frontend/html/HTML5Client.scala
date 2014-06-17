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
 * An HTML5 client with a DetailsPane. In the very common case of a tree controlled by XSEdit, use HTML5Client which extends this.
 */
abstract class HTML5ClientBase { hbthis =>

  private[this] val cmdBuffer = new ClientMessageBuffer
  def sendMessage(msg:ClientMessage) { queueMessage(msg); flushMessages() }
  def queueMessage(msg:ClientMessage) { 
    cmdBuffer+=msg
   // println("Message "+msg)
  }
  def flushMessages() {
    //println("FlushMessages()")
    for (m<-cmdBuffer.get()) transport.sendMessage(m)
  }

  
  def toolbar:Option[XSToolBar]
  def locale:Locale
  def executionContext:ExecutionContext
  def xsedit:XSEdit
  
  
  
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

  def dispose() {}
  def getDraggedElement(subid:String) : Option[XSClipBoard] = None
  
    // FIXME add some time based sanity check that shuts down the client when the session expires to stop a memory leak. The client currently does a decent job of warning the server of a closed window, but crashes are still a possibility.
  // This is not a problem when using WebSockets.

  val session = new HTTPSession(new HTTPSessionWorker{
    override def receivedMessage(message:SimpleClientMessage) { process(message) }
    override def dispose() {
      xsedit.removeDetailsPane(detailsPane)
      xsedit.dependencyInjectionCleaningQueue.removeQueueEmptyListener(queueEmptyListener)
      xsedit.unregisterActiveEditor()
      hbthis.dispose() 
    }
    override def getDraggedElement(subid:String) : Option[XSClipBoard] = hbthis.getDraggedElement(subid)
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
      for (c<-detailsPane.detailsPaneProcessMessages::processMessages) {
        //println("Got message "+message)
        if (c.isDefinedAt(message)) {
          //println("Sent to custom")
          c(message)
          return
        }
      }
      message.command match {
        case "Toolbar" if args.length==1 =>
          args(0) match {
            case "undo" => xsedit.undo()
            case "redo" => xsedit.redo()
            case "save" => for (t<-toolbar) { t.onSave(); xsedit.undoRedo.reset(xsedit.currentObject); xsedit.updateToolbar() }
            case "revert" => for (t<-toolbar) t.onRevert()
            case _ =>
          }
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
  
  private[this] val toolbarIDprefix = session.sessionPrefix+"toolbar."


  val toolbarListener = new ToolbarStatusListener {
    private[this] var toolbarUndoCurrentValue : Option[String] = Some("")
    private[this] var toolbarRedoCurrentValue : Option[String] = Some("")
    private[this] var saveEnabledCurrentValue = true
    private[this] var revertEnabledCurrentValue = true
    private[this] var staleCurrentValue=false
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
        if (t.useSave) {
          if (saveEnabledCurrentValue!=status.dirty) {
            saveEnabledCurrentValue=status.dirty
            sendMessage(ClientMessage.setToolbarStatus(toolbarIDprefix+"save",status.dirty,"Save"))
          }
        }
        if (t.useRevert) {
          if (revertEnabledCurrentValue!=status.dirty) {
            revertEnabledCurrentValue=status.dirty
            sendMessage(ClientMessage.setToolbarStatus(toolbarIDprefix+"revert",status.dirty,"Revert"))
          }
        }        
        if (staleCurrentValue!=status.stale) {
          staleCurrentValue=status.stale
          sendMessage(ClientMessage.setToolbarStatus(toolbarIDprefix+"stale",status.stale,if (status.stale) (<em>While you were editing, the file was changed elsewhere.</em>).toString else ""))
        }
      }
    }
  }
  xsedit.addToolbarStatusListener(toolbarListener)
  
  
  
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

  
}
/**
 * The controller for communication with an HTML client. The details of the transport are not
 * included here - it could be via HTTP or Websockets.
 */
class HTML5Client(val xsedit:XSEdit,val toolbar:Option[XSToolBar],val locale:Locale,val executionContext:ExecutionContext) extends HTML5ClientBase {

    override def dispose() {
      xsedit.removeTreeListener(treeListener)
    }

    override def getDraggedElement(subid:String) : Option[XSClipBoard] = {
      treePane.ID.unapply(subid) match {
        case Some(node) => Some(xsedit.copyData(List(node.node)))
        case None => None // could conceivably get something from a table in the detailsPane.
      }
      
    }


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
      def isGhosted(node:XSTreeNode) : Boolean = false
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