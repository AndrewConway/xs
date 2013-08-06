/**
 * Copyright (c) 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.api.command

import org.greatcactus.xs.api.display.RichLabel
import org.greatcactus.xs.api.icon.IconManifests
import org.greatcactus.xs.api.icon.Icon

/**
 * Description of an Edit Command. Returned by a method that is annotated with @XSEditCommands
 */
trait EditCommandDescription {

  /** A description of this action used in the undo history */
  def undoDescription : String
  /** A description of this action used for the button in the UI */
  def label : RichLabel
  def tooltip : Option[RichLabel] = None
  def icon : Option[String] = None
  /** Perform the action. Produce a new value to replace the existing class that generated this. If it returns null, then nothing will change. */
  def run() : AnyRef
  
}

class EditCommandDescriptionMadeConcrete(source:EditCommandDescription) {
  def undoDescription : String = source.undoDescription
  val label : RichLabel = source.label
  val tooltip : Option[RichLabel] = source.tooltip
  val iconString : Option[String] = source.icon
  val icon : Option[Icon] = iconString.flatMap{s=>IconManifests.getIconManifests(source.getClass()).iconOfLogicalName(s)}

  def run() : AnyRef = source.run()

  def isSameUI(other:EditCommandDescriptionMadeConcrete) : Boolean = {
    undoDescription==other.undoDescription && label==other.label && tooltip == other.tooltip && iconString==other.iconString
  }
  
}