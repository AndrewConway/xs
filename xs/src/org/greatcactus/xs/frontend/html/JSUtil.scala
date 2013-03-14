/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.frontend.html

import com.fasterxml.jackson.core.JsonGenerator
import java.io.StringWriter

/**
 * @author Andrew
 *
 */
object JSUtil {
  /** Return a properly escaped javascript string representing an array of the elements in variable "from". The actual contents should be written in function f, which should write fields - the start and end object are already done. */
  def makeArray[T](from:Seq[T])(f:(T,JsonGenerator)=>Unit) : String = {
    val out = new StringWriter()
    val g = ClientMessage.jsonFactory.createGenerator(out)
    g.writeStartArray()
    for (c<-from) {
      g.writeStartObject()
      f(c,g)
      g.writeEndObject()
    }
    g.writeEndArray()
    g.close()
    out.toString()
  }
}
