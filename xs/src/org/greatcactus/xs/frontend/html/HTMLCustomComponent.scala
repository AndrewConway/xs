/**
 * Copyright (c) 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.frontend.html

import org.greatcactus.xs.frontend.CustomComponent
import org.greatcactus.xs.frontend.DetailsPaneFieldCustom
import scala.xml.NodeSeq
import org.greatcactus.xs.frontend.XSTreeNode
import org.greatcactus.xs.frontend.CustomComponentWork
import org.greatcactus.xs.frontend.XSDetailsPane

/**
 * A custom component for HTML. Implement this if you want to make a custom component for the HTML view.
 */
abstract class HTMLCustomComponent[S] extends CustomComponent[S,String] {
  /** 
   * Create the HTML for the initial value of the component. Updates will be handled by the HTMLCustomComponentWork class.  
   * This will be put inside a div with id=id+"_ui" and class "xsCustom"+this.name. May also add to creator.postCreationJavascript
   **/
  def createHTML(id:String,field:DetailsPaneFieldCustom,initialValue:S,creator:GUICreatorHTML5) : NodeSeq

  /** Like getWork, except with more appropriate types. Override this instead. */
  def getHTMLWork(pane:HTML5DetailsPane,id:String,initial:S) : HTMLCustomComponentWork[S]
  
  override def getWork(pane:XSDetailsPane[String],id:String,initial:S) : CustomComponentWork[S] = pane match {
    case htmlpane:HTML5DetailsPane => 
      val res = getHTMLWork(htmlpane,id,initial)
      htmlpane.addCustomControllerProcessMessages(id,res.processMessages)
      res
    case _ => throw new IllegalArgumentException(pane.toString)
  }

}

abstract class HTMLCustomComponentWork[S] extends CustomComponentWork[S] {
  /** 
   * Deal with a message from the client that is relevent for this component. This is the way for the user's edits to be passed back to the server.
   * Do not deal with (in the PartialFunction sense) any messages that are not for this component, or else they will be swallowed up before going to their proper place. 
   */ 
  def processMessages : PartialFunction[SimpleClientMessage,Unit]
}

