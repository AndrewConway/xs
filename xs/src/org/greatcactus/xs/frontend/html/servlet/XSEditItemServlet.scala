/**
 * Copyright 2012-2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.frontend.html.servlet

import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.greatcactus.xs.frontend.XSEdit
import java.util.Locale
import org.greatcactus.xs.frontend.html._
import scala.collection.mutable.Queue
import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedBlockingQueue
import org.greatcactus.xs.api.serialization.XMLDeserialize
import java.io.FileInputStream
import java.io.File
import org.greatcactus.xs.api.serialization.XMLSerialize
import java.io.FileOutputStream
import org.greatcactus.xs.api.icon.IconManifests
import org.greatcactus.xs.api.icon.URLOfIcon
import org.greatcactus.xs.api.icon.ResolvedIcon
import java.net.URLEncoder
import scala.concurrent.Await
import org.greatcactus.xs.frontend.XSToolBar
import scala.xml.NodeSeq

/**
 * A more specific case of XSServlet that just edits an object.
 * 
 * hasTree should be true if the tree on the lhs should be shown.
 */
abstract class XSEditItemServlet(val hasTree:Boolean) extends XSServlet {  
  
  /** The main front page, for a get command without a "sub" parameter. This is what the user should override. */
  override def mainPage(request:HttpServletRequest,response:HttpServletResponse) {
    mainPage(xsobj(request),response)
  }
    
  abstract class XSSI(val title:String,val obj:AnyRef) {
    def save(editingResult:AnyRef)
  }
  
  /** Main user defined function, that gets the object to be edited */
  def xsobj(request:HttpServletRequest) : XSSI
  
  def extraStylesheets : List[String] = Nil
  
  def mainPage(xssi:XSSI,response:HttpServletResponse) {
    response.setContentType("text/html");
    response.setCharacterEncoding("UTF-8")
    val out = response.getWriter();
    out.println("""<!DOCTYPE html>""")
    val xsEditor = new XSEdit(xssi.obj)
    object Toolbar extends XSToolBar {
      override def onSave() { xssi.save(xsEditor.currentObject)}
      override def onRevert() { xsEditor.replaceRoot(xssi.obj)}
      override def useRevert = true
    }
    val client = new HTML5Client(xsEditor,Some(Toolbar),Locale.ENGLISH,executionContext)
    val page =  
      <html>
        <head>
          <meta charset="utf-8"/>
          <link rel="stylesheet" href="SlickGrid/slick.grid.css"/>
          <link rel="stylesheet" href="jQueryUI/css/smoothness/jquery-ui.css"/>
          <link rel="stylesheet" href="jquery.contextMenu/jquery.contextMenu.css"/>
          <link rel="stylesheet" href="xs/xsedit.css"/>
          {
            for (ss<-extraStylesheets) yield <link rel="stylesheet" href={ss}/>
          }
          <script src="jQuery/jquery-1.9.1.min.js"> </script>
          <script src="jQuery/jquery-migrate-1.1.0.js"> </script>
          <script src="xs/xsedit.js"> </script>
          <script src="xs/xsPTF.js"> </script>
          <script src="xs/xsGrid.js"> </script>
          <script src="jQueryUI/js/jquery-ui-1.10.0.custom.min.js"></script>
          <script src="jQuery/jquery.event.drag-2.2.js"></script>
          <script src="jQuery/jquery.event.drop-2.2.js"></script>
          <script src="SlickGrid/slick.core.js"></script>
          <script src="SlickGrid/plugins/slick.cellrangedecorator.js"></script>
          <script src="SlickGrid/plugins/slick.cellrangeselector.js"></script>
          <script src="SlickGrid/plugins/slick.cellselectionmodel.js"></script>
          <script src="SlickGrid/plugins/slick.rowselectionmodel.js"></script>
          <script src="SlickGrid/plugins/slick.rowmovemanager.js"></script>
          <script src="SlickGrid/slick.formatters.js"></script>
          <script src="SlickGrid/slick.editors.js"></script>
          <script src="SlickGrid/slick.grid.js"></script>
          <script src="jquery.contextMenu/jquery.ui.position.js"></script>
          <script src="jquery.contextMenu/jquery.contextMenu.js"></script>
          <title>{xssi.title}</title>
        </head>
        <body spellcheck="false">
          
          <h1>Test XS</h1>
          { if (hasTree) client.baseHTML else client.toolbarHTML++client.justDetailsPaneHTML }
        </body>
      </html>
    //println(page.toString)
    out.println(page.toString)
  }
  
  /** May be overridden */
  def header(xssi:XSSI) : NodeSeq = NodeSeq.Empty
  /** May be overridden */
  def footer(xssi:XSSI) : NodeSeq = NodeSeq.Empty
  
}  