/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.frontend.html

import scala.xml.Attribute
import scala.xml.Text

/**
 * @author Andrew
 *
 */
object XSHTMLUtil {
  def possiblySetNoDisplay(node:xml.Elem,visible:Boolean) : xml.Elem = 
    if (visible) node else node%Attribute(None,"style",Text("display:none;"),scala.xml.Null)

}