/**
 * Copyright (c) 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.frontend

import java.util.Locale
import org.greatcactus.xs.api.errors.ResolvedXSError

/**
 * Custom component for when you need to do something mildly special. The standard editor
 * mostly works (e.g. string), but you want a button next to it that lets you have a custom
 * editor for it (e.g. date, file system browser).
 * 
 * You need to make an instance of this class
 * and register it with the client handler. For the usual case of the HTML5 client, then you need to
 * create a HTMLCustomPopupComponent, and register it with the HTML5DetailsPane.addCustom object.
 */
abstract class CustomPopupComponent {

  /** The name that is matched against the arguments to @CustomEditable */
  def name:String
  

}

