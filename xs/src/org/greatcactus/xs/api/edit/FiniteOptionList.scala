/**
 * Copyright (c) 2013 Silicon Econometrics Pty. Ltd. All rights reserved.
 */
package org.greatcactus.xs.api.edit

/**
 * If an object being edited is a subclass of FiniteOptionList, it means that it is effectively an enumeration.
 * It can be one of the latter types if helpful.
 * 
 * It should also have a companion object with a method apply that takes a string (result of option.toString) and produces an element, as well as a value/method allValues that produces a list of all possible values. 
 */
trait FiniteOptionList {
}

trait FiniteOptionListWithPrettyNames extends FiniteOptionList {
  /** Provide a human readable name for this element. */
  def humanReadableName : String
}

trait FiniteOptionListWithIcons extends FiniteOptionList {
  /** Provide a possibly null icon identifier */
  def icon : String
}

trait FiniteOptionListWithPrettyNamesAndIcons extends FiniteOptionListWithPrettyNames with FiniteOptionListWithIcons

