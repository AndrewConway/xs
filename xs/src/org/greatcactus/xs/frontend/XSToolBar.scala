/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.frontend

/**
 * Should be implemented by clients that want to use the XS tool bar, and should be overridden with the actual commands.
 */
class XSToolBar {
  def useUndoRedo : Boolean = true
  def useSave : Boolean = true
  def useRevert : Boolean = false
  def others : List[String] = Nil
  
  def onSave() {}
  def onRevert() {}
  def onOther(cmd:String) {}
}