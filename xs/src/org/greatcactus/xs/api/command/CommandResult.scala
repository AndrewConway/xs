/**
 * Copyright (c) 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.api.command

import org.greatcactus.xs.api.display.RichLabel

/**
 * The result of an XS command - whether it worked, and optional information
 */
class CommandResult(val success:Boolean,val info:Option[RichLabel]) {

}

object CommandResult {
  val ok = new CommandResult(true,None)
  def success(label:RichLabel) = new CommandResult(true,Some(label))
  def success(label:String) = new CommandResult(true,Some(RichLabel(label)))
  def failure(label:RichLabel) = new CommandResult(false,Some(label))
  def failure(label:String) = new CommandResult(false,Some(RichLabel(label)))
}

class CommandError(val description:String) extends Exception(description)
