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
import org.greatcactus.xs.frontend.DetailsPaneField
import org.greatcactus.xs.frontend.CustomPopupComponent

/**
 * A custom popup component for HTML. Implement this if you want to make a custom popup component for the HTML view.
 */
abstract class HTMLCustomPopupComponent extends CustomPopupComponent {
  /** 
   * Create the HTML and some post-creation javascript for the dialog. This will be opened inside a JqueryUI dialog.
   * The HTML will be created inside a div with the stated id followed by "_popup". Internal ids can be based upon this one (i.e., prefixed by id, with a suffix of "_popup_whateveryouwant").
   * 
   * When done it should call ondone, with None meaning it was cancelled.
   **/
  def createHTML(id:String,field:DetailsPaneField,initialValue:String,pane:HTML5DetailsPane,dependencies:() => Set[AnyRef]) : HTMLCustomPopupComponentInfo

  

}

class HTMLCustomPopupComponentInfo(
    /** The base id of the field containing the popup */
    val id:String,
    /** The HTML that will apprear inside the dialog */
    val html:NodeSeq,
    /** Javascript that will be called after the said HTML has been created */ 
    val postCreationJS:List[ClientMessage],
    /** When evaluated, a javascript function that will return null if OK, otherwise an error message */
    val okJSFunction:String,
    /** When evaluated, a javascript function that will return a serialized version of the result */
    val resultJSFunction:String,
    /** a server side function that takes the string returned from resultJSFunction and produces a modified result */
    val serverSideProcessResultFunction : Option[String=>String],
    /** An optional function that should get called on the server when the server closes the popup (for instance, changes what is being edited) */
    val closedByServer:Option[()=>Unit],
    /** An optional function that should get called on the server when the client closes the popup */
    val closedByClient:Option[()=>Unit],
    /** An optional function that processes client messages */
    val processMessages:Option[PartialFunction[SimpleClientMessage,Unit]]
) {
 
}