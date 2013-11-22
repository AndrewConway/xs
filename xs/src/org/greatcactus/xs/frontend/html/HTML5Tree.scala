/**
 * Copyright 2012-2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.frontend.html

import java.util.Locale
import scala.xml.NodeSeq
import scala.xml.Text
import org.greatcactus.xs.util.EqualityByPointerEquality
import scala.collection.mutable.ListBuffer
import scala.xml.Attribute
import org.greatcactus.xs.api.display.RichLabel
import java.util.prefs.Base64
import org.greatcactus.xs.frontend.XSClipBoard

/**
 * A general purpose hierarchical HTML/javascript/ajax tree.
 * Usage:
 *   1) Provide a transport to send messages back to the client
 *   2) Provide a model which describes the tree (of elements of type T). 
 *      Somewhat surprisingly, this model contains also information about what is open and what is closed.
 *   3) Provide the root of the tree
 *   4) Call the baseHTML() function once to get the initial HTML for the tree.
 *   5) Include appropriate Javascript and CSS files (xsedit.js and xsedit.css)
 *   6) When a node changes in any way (open/closed, label, icon, children), call the refresh() function with that node.
 */
class HTML5Tree[T <: AnyRef](val locale:Locale,val transport:HTMLTransport,val model:TreeModel[T],val root:T,val sessionprefix:String,val treeID:String,val allowMultipleSelection:Boolean,val allowDragging:Boolean,val initialSelection:Option[T]=None) {

  val treeDivID = "xsTree"+treeID
  val treePrefix = "xsTreeNode"+treeID+"_"  // tree ID added to make unique in page. This is often the session ID.
  def sessionprefixNoTrailingPeriod = sessionprefix.substring(0,sessionprefix.length-1)
    
  var currentlySelected : Option[T] = initialSelection.orElse(Some(root))
  var alsoSelected : List[T] = Nil

  def getAllSelected : List[T] = currentlySelected match {
    case Some(n) => n::alsoSelected
    case None => alsoSelected
  }
    
  var clientNodes : Map[String,OnClientTreeNode] = Map.empty
  
  def newHTML(node:T,clicksAwayFromVisible:Int,postCreationJavascript:ListBuffer[String]) : (NodeSeq,OnClientTreeNode) = {
    val id = treePrefix+model.uniqueID(node)
    val isOpen = model.isOpen(node)
    val children = model.children(node)
    val isOpenString = if (children.isEmpty) "" else if (isOpen) "▼" else "►"
    //println("isOpenString = "+isOpenString+" length="+isOpenString.length)
    val opener = <span id={id+"_opener"} class="xsOpener" onclick={sessionprefix+"treeOpen('"+id+"'); return false"}>{isOpenString}</span>
    val iconURL = model.icon(node)
    val icon = iconURL match {
      case Some(url) => <img id={id+"_icon"} src={url} class="xsIcon"/>
      case None => NodeSeq.Empty
    }
    val labelS = model.title(node)
    val label = <span id={id+"_label"} class="xsTreeNodeName">{labelS.htmlNotBlank}</span>
    postCreationJavascript++=labelS.postCreationJavascript
    val childrenClicksAwayFromVisible = clicksAwayFromVisible+(if (isOpen) 0 else 1)
    val (kidsHTML,kidsLocal) = 
      if (childrenClicksAwayFromVisible<=clicksAwayFromVisibleCutoff) children.map{newHTML(_,childrenClicksAwayFromVisible,postCreationJavascript)}.unzip
      else (Nil,Nil)
    val subsVisUnchecked = <ul id={id+"_subs"}>{kidsHTML.toList}</ul>
    val subs = XSHTMLUtil.possiblySetNoDisplay(subsVisUnchecked, isOpen)
    val errorlevel = model.errorLevel(node)
    val erroricon = <span id={id+"_erroricon"} class={"xsErrorIcon xsErrorIcon"+errorlevel}></span>
    val isCurrentlySelected = getAllSelected.contains(node)
    val isCurrentlyGhosted = model.isGhosted(node)
    val selectableClass : String = "xsTreeSelectable"+(if (isCurrentlySelected) " xsSelected" else "")+(if (isCurrentlyGhosted) " xsGhosted" else "")
    val selectable = <span id={id+"_selectable"} class={selectableClass} onclick={sessionprefix+"treeSelect(event,'"+id+"'); return false"} oncontextmenu={sessionprefix+"treeSelect(event,'"+id+"'); return true"}>{erroricon}{icon}{label}</span>
    //val selectable = if (isCurrentlySelected) selectableNoSelection%Attribute(None,"class",Text("xsTreeSelectable xsSelected"),scala.xml.Null)  else selectableNoSelection
    val html = <div id={id+"_all"} draggable={allowDragging.toString} class="xsTreeNodeCompleteNode">{opener}{selectable}{subs}</div>
    val localnode = new OnClientTreeNode(iconURL,labelS,isOpenString,kidsLocal,clicksAwayFromVisible,!isOpen,isCurrentlySelected,isCurrentlyGhosted,errorlevel,node,id)
    clientNodes+=id->localnode
    (html,localnode)
  }
  
  /** This should only be called once */
  def baseHTML(divClasses:String="") : NodeSeq = {
    if (clientNodes.isEmpty) {
      val postCreationJavascript=new ListBuffer[String]
      val res= <div class={"xsEditTree "+divClasses} id={treeDivID} data-multiselect={allowMultipleSelection.toString} data-oninputobj={sessionprefixNoTrailingPeriod} ondragstart={sessionprefix+"dragStart(event);"} ondragend={sessionprefix+"dragEnd(event);"} ondragover={sessionprefix+"dragOver(event)"} ondragenter={sessionprefix+"dragEnter(event)"} ondragleave={sessionprefix+"dragLeave(event)"} ondrop={sessionprefix+"drop(event)"}>{newHTML(root,0,postCreationJavascript)._1}</div>
      for (cmd<-postCreationJavascript) transport.sendMessage(ClientMessage.run(cmd)) // this doesn't look like post-creation, but actually is because the tree is created and then it starts the comet which gets these things. However, if this is called a second time, bad things will happen. Thus javadoc comment about only calling once.
      res
    }
    else throw new IllegalArgumentException("Called baseHTML more than once.")
  }
  
  object ID { // used in processMessages for extracting the node given a string id
    def unapply(id:String) : Option[OnClientTreeNode] = clientNodes.get(id)
  }
  object Bool {
    def unapply(arg:String) : Option[Boolean] = Some(arg.toBoolean)
  }
  
  val processMessages : PartialFunction[SimpleClientMessage,Unit] = {
    case SimpleClientMessage("TreeOpen",Array(ID(n),Bool(shouldNowBeOpen))) =>
      model.userToggledStatus(n.node,shouldNowBeOpen)
    case SimpleClientMessage("TreeSelect",Array(ID(n),Bool(added))) =>
      setSelected(Some(n.node),added)
      model.userSelected(n.node) 
    case SimpleClientMessage("TreeContextMenu",Array(this.treeDivID,command,commaSeparatedSelected)) =>
      val nodes = commaSeparatedSelected.split(',').flatMap{clientNodes.get(_)}.map{_.node}
      model.userContextMenu(command,nodes)
    case SimpleClientMessage("TreeDragLocal",Array(ID(source),ID(dest),Bool(isAbove))) =>
      model.dragLocal(source.node,dest.node,isAbove)
    case SimpleClientMessage("TreeFileDrag",Array(ID(dest),Bool(isAbove),urlEncodedContents,filename,lastModified)) =>
      val commapos = urlEncodedContents.indexOf(",")
      if (commapos>=0) {
        val contents = new sun.misc.BASE64Decoder().decodeBuffer(urlEncodedContents.substring(commapos+1))
        model.dragInFile(dest.node,isAbove,contents,filename,try {Some(lastModified.toLong)} catch { case _:NumberFormatException => None })
      }
      // println("TreeFileDrag of "+filename+" last modified "+lastModified+"  contents: "+base64encodedcontents) 
    case SimpleClientMessage("TreeDragNonLocal",Array(source,ID(dest),Bool(isAbove))) =>
      val NodeParser = """xsTreeNode(\d+)_(\d+)_all""".r
      //println("TreeDragNonLocal "+source) 
      source match {
        case NodeParser(sessionid,nodeid) =>
          //println("Session ID "+sessionid) 
          SessionManagement.get(sessionid) match {
            case Some(session) =>
              //println("Found session")
              for (clip<-session.worker.getDraggedElement("xsTreeNode"+sessionid+"_"+nodeid)) { // remove the _all
                //println("Got clip "+clip)
                model.nonLocalDrag(dest.node,isAbove,clip) 
              }
            case None =>
          }
        case _ =>
      }
      //model.dragLocal(source.node,dest.node,isAbove)
  }

  
  /** Called when something changes on the given node (eg open/closed/kids/title/icon) */
  def refresh(node:T) {
    val id = treePrefix+model.uniqueID(node)
    // println("Refreshing "+id)
    for (cn<-clientNodes.get(id)) {
      cn.setIcon(model.icon(node))
      cn.setLabel(model.title(node))
      cn.setIsOpen()
      cn.setIsSelected()
      cn.setIsGhosted()
      cn.setErrorLevel(model.errorLevel(node))
    }
  }

  
  def setSelected(newSelected:Option[T],addTo:Boolean) {
    //println("Set selected "+newSelected+" addTo="+addTo)    
    synchronized {
      if (newSelected!=currentlySelected) {
        val oldSelected=getAllSelected
        alsoSelected=if (addTo) {
          val longer = if (currentlySelected.isDefined) currentlySelected.get::alsoSelected else alsoSelected
          newSelected match {
            case Some(e) => longer.filter{_ != e}
            case None => longer
          }
        } else Nil
        //println("Set selected "+newSelected+" addTo="+addTo+"  alsoSelected="+alsoSelected)    
        currentlySelected=newSelected
        for (n<-newSelected) refresh(n) 
        for (n<-oldSelected) refresh(n) 
      }
    }
  }
  
  
  /** Children nodes are maintained on the client (for snappy response) if the number of clicks needed to see them is this or less. Small numbers (eg 0) mean low bandwidth; large numbers (eg 1,2) mean low latency */
  val clicksAwayFromVisibleCutoff = 1
  
  class OnClientTreeNode(var iconURL:Option[String],var label:RichLabel,var isOpenString:String,var children:Seq[OnClientTreeNode],var clicksAwayFromVisible:Int,var childrenCurrentlyHidden:Boolean,var isCurrentlySelected:Boolean,var isCurrentlyGhosted:Boolean,var currentErrorLevel:Int,val node:T,val id:String) {
    def setIcon(newURL:Option[String]) {
      //println("Set icon "+newURL)
      if (newURL!=iconURL) {
        val iconid = id+"_icon"
        if (iconURL.isDefined) newURL match {
          case Some(url) => transport.sendMessage(ClientMessage.setAttributeID(iconid,"src",url))
          case None => transport.sendMessage(ClientMessage.removeID(iconid))
        } else transport.sendMessage(ClientMessage.addAfterID(id+"_erroricon", <img id={iconid} src={newURL.get} class="xsIcon"/>)) 
        iconURL=newURL
      }
    }
    def setErrorLevel(newErrorLevel:Int) {
      if (newErrorLevel!=currentErrorLevel) {
        transport.sendMessage(ClientMessage.setAttributeID(id+"_erroricon","class", "xsErrorIcon xsErrorIcon"+newErrorLevel))
        currentErrorLevel=newErrorLevel
      }
    }
    def setLabel(newlabel:RichLabel) {
      if (newlabel!=label) {
        transport.sendMessage(ClientMessage.setHTMLIDnotBlank(id+"_label",newlabel))
        label=newlabel
      }
    }
    def setIsSelected() {
      val shouldBeSelected = getAllSelected.contains(node)
      if (shouldBeSelected!=isCurrentlySelected) {
        isCurrentlySelected=shouldBeSelected
        transport.sendMessage(ClientMessage.addClass("#"+id+"_all > span:nth-child(2)","xsSelected",shouldBeSelected))
      }
    }
    def setIsGhosted() {
      val shouldBeGhosted = model.isGhosted(node)
      if (shouldBeGhosted!=isCurrentlyGhosted) {
        isCurrentlyGhosted=shouldBeGhosted
        transport.sendMessage(ClientMessage.addClass("#"+id+"_all > span:nth-child(2)","xsGhosted",shouldBeGhosted))
      }
    }
    def isOpenOnClient = isOpenString=="▼"
    def setIsOpen() {
      val newIsOpen = model.isOpen(node)
      val newChildren = model.children(node)
      val newIsOpenString = if (newChildren.isEmpty) "" else if (newIsOpen) "▼" else "►"
      if (newIsOpenString!=isOpenString) {
        transport.sendMessage(ClientMessage.setHTMLID(id+"_opener",Text(newIsOpenString)))
        isOpenString=newIsOpenString
      }
      val childrenClicksAwayFromVisible = clicksAwayFromVisible+(if (newIsOpen) 0 else 1)
      if ((!newIsOpen)!=childrenCurrentlyHidden) {
        transport.sendMessage(ClientMessage.setVisibleID(id+"_subs",newIsOpen))
        childrenCurrentlyHidden= !newIsOpen
      }
      if ((childrenClicksAwayFromVisible<=clicksAwayFromVisibleCutoff || !children.isEmpty) && !EqualityByPointerEquality.listsEqualByPointerEquality(newChildren,children.map{_.node})) {
        // need to check that the children are all present. Can assume correct if present.
        // The last check is redundant but is a fast-ok for common case efficiency
        val newChildrenAsSet = newChildren.map{new EqualityByPointerEquality(_)}.toSet
        val (keepChildren,discardChildren) = children.partition(c=>newChildrenAsSet.contains(new EqualityByPointerEquality(c.node)))
        for (d<-discardChildren) d.discard()
        var availableChildren = keepChildren
        var lastGoodNode : Option[String] = None
        val addMessages = new ListBuffer[ClientMessage]
        children=for (c<-newChildren) yield {
          val resnode = availableChildren match {
            case h::t if h.node==c =>
              availableChildren=t
              h
            case _ =>
              val postCreationJavascript = new ListBuffer[String]
              val (html,kid) = newHTML(c,childrenClicksAwayFromVisible,postCreationJavascript)
              addMessages+=(lastGoodNode match {
                case None => ClientMessage.addAtStartID(id+"_subs",html)
                case Some(afterid) => ClientMessage.addAfterID(afterid+"_all",html)
              })
              for (cmd<-postCreationJavascript) addMessages+=ClientMessage.run(cmd)
              kid
          }
          lastGoodNode=Some(resnode.id)
          resnode
        }
        for (a<-availableChildren) a.discard()
        for (m<-addMessages) transport.sendMessage(m) // may need to have these messages after the discards if nodes are reordered
      }
      for (c<-children) c.setClicksAwayFromVisible(childrenClicksAwayFromVisible)
      
    }
    def setClicksAwayFromVisible(newClicksAwayFromVisible:Int) {
      if (clicksAwayFromVisible!=newClicksAwayFromVisible) {
        clicksAwayFromVisible=newClicksAwayFromVisible
        setIsOpen()
      }
      
    }
    def discard() {
      transport.sendMessage(ClientMessage.removeID(id+"_all"))
      removeRecursivelyFromMap()
    }
    private def removeRecursivelyFromMap() {
      clientNodes.get(id) match { // need to check that the thing being removed is actually still current, in case of node reordering.
        case Some(c) if c eq this => clientNodes-=id 
        case _ =>
      }
      for (c<-children) c.removeRecursivelyFromMap()
    }
  }

}
/*
object HTML5Tree {
  val blankImg = "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw=="
}
*/

abstract class TreeModel[T] {
  def children(node:T) : Seq[T]
  def title(node:T) : RichLabel
  def icon(node:T) : Option[String]
  /** 0=error, 1=warning, 2=info, 1000 = no error */
  def errorLevel(node:T) : Int 
  def userToggledStatus(node:T,isNowOpen:Boolean)
  def userSelected(node:T)
  /** Return a string unique amongst all nodes */
  def uniqueID(node:T) : String
  def isOpen(node:T) : Boolean
  def isGhosted(node:T) : Boolean
    /** Called by the client when one node is dragged onto another. isAbove is true if wants to be above the given node instead of on it. */
  def dragLocal(source:T,dest:T,isAbove:Boolean)
    /** The user has chosen a context menu with particular nodes selected. */
  def userContextMenu(command:String,nodes:Seq[T])
    /** The user has dragged and dropped a file */
  def dragInFile(dest:T,isAbove:Boolean,contents:Array[Byte],filename:String,lastModified:Option[Long])
    /** User has dragged, onto here, something from another window */
  def nonLocalDrag(dest:T,isAbove:Boolean,clip:XSClipBoard)

}