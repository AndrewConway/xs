/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.util

import com.fasterxml.jackson.core.JsonFactory
import javax.servlet.http.HttpServletResponse

/**
 * Some general JSON utilities not actually necessarily used by XS but likely to be useful.
 */
object JSONUtil {
  val jsonFactory = new JsonFactory()
  
  /** 
   *  Return a scala object as the result of a servlet call, JSON encoded.
   *  Valid types are: 
   *     A list or array or valid types, encoded as JSON array
   *     A map from strings to valid types, encoded as JSON object
   *     primitives, encoded as JSON primitives
   *     null, enconded as JSON null
   *     other, encoded as JSON string
   *     
   **/
  def toJSON(toSerialize:Any,response:HttpServletResponse) {
    response.setContentType("text/json");
    val out = response.getWriter();
    val g = jsonFactory.createJsonGenerator(out)
    def dowork(work:Any) {
      work match {
        case null => g.writeNull()
        case map:Map[_,_] =>
          g.writeStartObject()
          for ((key,value)<-map) {
            g.writeFieldName(key.toString)
            dowork(value)
          }
          g.writeEndObject()
        case seq:Seq[_] =>
          g.writeStartArray()
          for (v<-seq) dowork(v)
          g.writeEndArray()
        case seq:Array[_] =>
          g.writeStartArray()
          for (v<-seq) dowork(v)
          g.writeEndArray()
        case s:String => g.writeString(s)
        case n:Int => g.writeNumber(n)
        case n:Short => g.writeNumber(n)
        case n:Byte => g.writeNumber(n)
        case n:Double => g.writeNumber(n)
        case n:Float => g.writeNumber(n)
        case b:Boolean => g.writeBoolean(b)
        case c:Char => g.writeString(c.toString)
        case other => g.writeString(other.toString)
      }
    }
    dowork(toSerialize)
    g.close()
  }
  
}