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

/**
 * A general purpose hierarchical HTML/javascript/ajax tree.
 * Usage:
 *   1) Provide a transport to send messages back to the client
 *   2) Provide a model which describes the tree (of elements of type T). 
 *      Somewhat surprisingly, this model contains also information about what is open and what is closed.
 *   3) Provide the root of the tree
 *   4) Call the baseHTML() function once to get the initial HTML for the tree.
 *   5) Include appropriate Javascript and CSS files (TODO enumerate)
 *   6) When a node changes in any way (open/closed, label, icon, children), call the refresh() function with that node.
 */
class HTML5Tree[T <: AnyRef](val locale:Locale,val transport:HTMLTransport,val model:TreeModel[T],val root:T,val sessionprefix:String,val sessionID:String) {

  val treePrefix = "xsTreeNode"+sessionID+"_"  // session ID added to make unique in page.
  def sessionprefixNoTrailingPeriod = sessionprefix.substring(0,sessionprefix.length-1)
    
  var currentlySelected : Option[T] = Some(root)

    
  var clientNodes : Map[String,OnClientTreeNode] = Map.empty
  
  def newHTML(node:T,clicksAwayFromVisible:Int,postCreationJavascript:ListBuffer[String]) : (NodeSeq,OnClientTreeNode) = {
    val id = treePrefix+model.uniqueID(node)
    val isOpen = model.isOpen(node)
    val isOpenString = if (model.children(node).isEmpty) "" else if (isOpen) "▼" else "►"
    //println("isOpenString = "+isOpenString+" length="+isOpenString.length)
    val opener = <span id={id+"_opener"} class="xsOpener" onclick={sessionprefix+"treeOpen('"+id+"'); return false"}>{isOpenString}</span>
    val iconURL = model.icon(node)
    val icon = iconURL match {
      case Some(url) => <img id={id+"_icon"} src={url} class="xsIcon"/>
      case None => NodeSeq.Empty
    }
    val labelS = model.title(node)
    val label = <span id={id+"_label"}>{labelS.htmlNotBlank}</span>
    postCreationJavascript++=labelS.postCreationJavascript
    val childrenClicksAwayFromVisible = clicksAwayFromVisible+(if (isOpen) 0 else 1)
    val (kidsHTML,kidsLocal) = 
      if (childrenClicksAwayFromVisible<=clicksAwayFromVisibleCutoff) model.children(node).map{newHTML(_,childrenClicksAwayFromVisible,postCreationJavascript)}.unzip
      else (Nil,Nil)
    val subsVisUnchecked = <ul id={id+"_subs"}>{kidsHTML.toList}</ul>
    val subs = XSHTMLUtil.possiblySetNoDisplay(subsVisUnchecked, isOpen)
    val errorlevel = model.errorLevel(node)
    val erroricon = <span id={id+"_erroricon"} class={"xsErrorIcon xsErrorIcon"+errorlevel}></span>
    val selectableNoSelection = <span id={id+"_selectable"} class="xsTreeSelectable" onclick={sessionprefix+"treeSelect('"+id+"'); return false"} oncontextmenu={sessionprefix+"treeSelect('"+id+"'); return true"}>{erroricon}{icon}{label}</span>
    val isCurrentlySelected = Some(node)==currentlySelected
    val selectable = if (isCurrentlySelected) selectableNoSelection%Attribute(None,"class",Text("xsTreeSelectable xsSelected"),scala.xml.Null)  else selectableNoSelection
    val html = <div id={id+"_all"} draggable="true">{opener}{selectable}{subs}</div>
    val localnode = new OnClientTreeNode(iconURL,labelS,isOpenString,kidsLocal,clicksAwayFromVisible,!isOpen,isCurrentlySelected,errorlevel,node,id)
    clientNodes+=id->localnode
    (html,localnode)
  }
  
  /** This should only be called once */
  def baseHTML() : NodeSeq = {
    if (clientNodes.isEmpty) {
      val postCreationJavascript=new ListBuffer[String]
      val res= <div class="xsEditTree" id={"xsTree"+sessionID} data-oninputobj={sessionprefixNoTrailingPeriod} ondragstart={sessionprefix+"dragStart(event);"} ondragend={sessionprefix+"dragEnd(event);"} ondragover={sessionprefix+"dragOver(event)"} ondragenter={sessionprefix+"dragEnter(event)"} ondragleave={sessionprefix+"dragLeave(event)"} ondrop={sessionprefix+"drop(event)"}>{newHTML(root,0,postCreationJavascript)._1}</div>
      for (cmd<-postCreationJavascript) transport.sendMessage(ClientMessage.run(cmd))
      res
    }
    else throw new IllegalArgumentException("Called baseHTML more than once.")
  }
  
  
  /** Called by the client when the opener is clicked on for a given id */
  def clickedOnOpener(id:String,shouldBeNowOpen:Option[Boolean]) {
    //println("Clicked on opener for "+id)
    for (n<-clientNodes.get(id)) {
      val open = shouldBeNowOpen.getOrElse(!n.isOpenOnClient)
      //println("Changing status to "+ open)
      model.userToggledStatus(n.node,open)
    } 
  }

    /** Called by the client when the opener is clicked on for a given id */
  def clickedOnNode(id:String) {
    //println("Clicked on node for "+id)
    for (n<-clientNodes.get(id)) {
        model.userSelected(n.node)
    } 
  }
  
  /** Called by the gui client when a context menu is chosen */
  def contextMenu(command:String,nodeids:Seq[String]) {
    var nodes = nodeids.flatMap{clientNodes.get(_)}.map{_.node}
    model.userContextMenu(command,nodes)
  }
  
  /** Called by the client when one node is dragged onto another. isAbove is true if wants to be above the given node instead of on it. */
  def dragLocal(idSource:String,idDest:String,isAbove:Boolean) {
    for (source<-clientNodes.get(idSource);dest<-clientNodes.get(idDest)) {
      model.dragLocal(source.node,dest.node,isAbove)
    }
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
      cn.setErrorLevel(model.errorLevel(node))
    }
  }

  
  def setSelected(newSelected:Option[T]) {
    synchronized {
      if (newSelected!=currentlySelected) {
        val oldSelected=currentlySelected
        currentlySelected=newSelected
        for (n<-newSelected) refresh(n) 
        for (n<-oldSelected) refresh(n) 
      }
    }
  }
  
  
  /** Children nodes are maintained on the client (for snappy response) if the number of clicks needed to see them is this or less. Small numbers (eg 0) mean low bandwidth; large numbers (eg 1,2) mean low latency */
  val clicksAwayFromVisibleCutoff = 1
  
  class OnClientTreeNode(var iconURL:Option[String],var label:RichLabel,var isOpenString:String,var children:Seq[OnClientTreeNode],var clicksAwayFromVisible:Int,var childrenCurrentlyHidden:Boolean,var isCurrentlySelected:Boolean,var currentErrorLevel:Int,val node:T,val id:String) {
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
      val shouldBeSelected = Some(node)==currentlySelected
      if (shouldBeSelected!=isCurrentlySelected) {
        isCurrentlySelected=shouldBeSelected
        transport.sendMessage(ClientMessage.addClass("#"+id+"_all > span:nth-child(2)","xsSelected",shouldBeSelected))
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
    /** Called by the client when one node is dragged onto another. isAbove is true if wants to be above the given node instead of on it. */
  def dragLocal(source:T,dest:T,isAbove:Boolean)
    /** The user has chosen a context menu with particular nodes selected. */
  def userContextMenu(command:String,nodes:Seq[T])
}