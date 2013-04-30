/**
 * Copyright (c) 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.frontend

import java.util.Locale
import org.greatcactus.xs.api.errors.ResolvedXSError

/**
 * Custom component for when you need to do something special. You need to make an instance of this class
 * and register it with the client handler. For the usual case of the HTML5 client, then you need to
 * create a HTMLCustomComponent, and register it with the HTML5DetailsPane.addCustom object.
 */
abstract class CustomComponent[S,T] {

  /** The name that is matched against the arguments to @CustomEditable */
  def name:String
  
  /** All the data from the source object that is needed for the painter. It is not clear that this is all that useful some times. */
  def state(node:XSTreeNode,locale:Locale) : S
  

  /** Create an object that does most of the work of updating the custom component */
  def getWork(pane:XSDetailsPane[T],gui:T,initial:S) : CustomComponentWork[S]

}

/** An object that contains all the information needed to deal with updates and edits for the custom component */
abstract class CustomComponentWork[S] {
  /** Change the value displayed in the custom component */
  def refresh(newvalue:S)
  /** Deallocate any resources associated with this component */
  def dispose()
     /** XS sending a command to the GUI to change the errors shown for a field. These errors all contain non-empty customComponentInformation. */
  def changeErrors(errors:List[ResolvedXSError])

}

/** A helper for registering custom components. Subclassed by the actual stores. */
class CustomComponentStore[T] {
  private var customs : List[CustomComponent[_,T]] = Nil
  def addCustom(custom:CustomComponent[_,T]) { customs=custom::customs}
  def getCustom(f:DetailsPaneFieldCustom) : Option[CustomComponent[_,T]] = customs.find(_.name==f.customComponentName)  
}
