/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.test

import org.greatcactus.xs.frontend.html.HTMLCustomPopupComponent
import org.greatcactus.xs.frontend.DetailsPaneField
import org.greatcactus.xs.frontend.html.HTMLCustomPopupComponentInfo
import org.greatcactus.xs.frontend.html.HTML5DetailsPane

/**
 * A silly demonstration of a custom popup. Just adds "Fred" to the text and
 * shows a simple form.
 */
object DemoPopup  extends HTMLCustomPopupComponent {
  def name = "DemoPopup"
  
  def createHTML(id:String,field:DetailsPaneField,initialValue:String,pane:HTML5DetailsPane,dependencies:() => Set[AnyRef]) : HTMLCustomPopupComponentInfo = {
    val html = <span>This is a popup editor <input type="text" id={id+"_popup_input"} value={initialValue+"Fred"}></input></span>
    val ok = "function() { return null; }"
    val jsValue = "function() { return document.getElementById('"+id+"_popup_input').value; }"
    new HTMLCustomPopupComponentInfo(id,html,Nil,ok,jsValue,None,None,None,None)
  }

}