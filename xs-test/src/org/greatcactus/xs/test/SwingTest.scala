/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.test
import swing._
import org.greatcactus.xs.frontend.XSEdit
import java.util.Locale
import org.greatcactus.xs.frontend.swing.SwingDetailsPane
import org.greatcactus.xs.frontend.swing.SwingTreePane

/**
 * Test the Swing XS implementation
 */
object SwingTest extends SimpleSwingApplication {

  val base = new Space(new History(""),Nil)
  val edit = new XSEdit(base)
  val details = new SwingDetailsPane(Locale.FRANCE,edit)
  edit.addDetailsPane(details)
  val treePanel = new SwingTreePane(edit)
  edit.addTreeListener(treePanel)
  details.panel.preferredSize=new Dimension(400,600)
  treePanel.wrapped.preferredSize=new Dimension(300,600)
  
  def top = new MainFrame {
    title = edit.getTitle(Locale.FRANCE).getOrElse("Edit test")
    contents = new SplitPane(Orientation.Vertical,treePanel.wrapped,details.panel)
  }
}