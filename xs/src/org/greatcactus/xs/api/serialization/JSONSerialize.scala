/**
 * Copyright Andrew Conway 2012-2014. All rights reserved.
 */
package org.greatcactus.xs.api.serialization

import java.io.PrintWriter
import org.greatcactus.xs.impl.SerializableTypeInfo
import scala.collection.GenTraversable
import java.io.StringWriter
import java.io.OutputStream
import javax.xml.stream.XMLStreamWriter
import javax.xml.stream.XMLOutputFactory
import java.io.ByteArrayOutputStream
import scala.collection.mutable.ArrayBuffer
import org.greatcactus.xs.impl.CollectionStringUtil
import org.greatcactus.xs.util.EqualityByPointerEquality
import org.greatcactus.xs.frontend.html.ClientMessage
import com.fasterxml.jackson.core.JsonGenerator
import java.io.Writer

/**
 * Serialize to JSON. This is slightly simpler than XML.
 * 
 * A class is serialized with fields for all its non-empty fields, plus one "xsType" with the name of the class being serialized.
 * The "xsType" field is omitted if there are no XS superclasses of the class being serialized (in which case it is not needed) unless alwaysWriteTypeTag=true.
 * Fields are serialized as follows:
 *   - non-XS fields are serialized as JSON primitives (as in XML serialization)
 *   - non-empty collections (other than option) are serialized as a JSON array
 *   - non-null object fields, or non-empty options, are serialized as a JSON object.
 *  Polymorphism is dealt with by the xsType field.
 */
object JSONSerialize {

  val typeTag = "xsType" // field for types. 
    
  def serializeToByteArray(obj:Any) : Array[Byte] = {
    val w = new ByteArrayOutputStream
    serialize(obj,w,false)
    w.toByteArray()
  }
  
  def serializeToString(obj:Any) : String = {
    val w = new StringWriter
    serialize(obj,w,false)
    w.toString
  }
  
  def serialize(obj:Any,out:OutputStream,alwaysWriteTypeTag:Boolean) {
    val g = ClientMessage.jsonFactory.createJsonGenerator(out)
    serialize(obj,g,alwaysWriteTypeTag)
    g.close()
  }

  def serialize(obj:Any,out:Writer,alwaysWriteTypeTag:Boolean) {
    val g = ClientMessage.jsonFactory.createJsonGenerator(out)
    serialize(obj,g,alwaysWriteTypeTag)
    g.close()
  }

  def serialize(toSerialize:Any,g:JsonGenerator,alwaysWriteTypeTag:Boolean)  {
    toSerialize match {
      case null => g.writeNull()
      case a:Array[_] => 
        g.writeStartArray()
        for (s<-a) serialize(s,g,alwaysWriteTypeTag)
        g.writeEndArray()
      case m:Map[_,_] =>
        g.writeStartObject()
        for ((key,value)<-m) {
          g.writeFieldName(key.toString)
          serialize(value,g,alwaysWriteTypeTag)
        }
        g.writeEndObject()
      case a:GenTraversable[_] => 
        g.writeStartArray()
        for (s<-a) serialize(s,g,alwaysWriteTypeTag)
        g.writeEndArray()
      case n:Int => g.writeNumber(n)
      case n:Short => g.writeNumber(n)
      case n:Double => g.writeNumber(n)
      case n:Float => g.writeNumber(n)
      case n:Byte => g.writeNumber(n)
      case n:Char => g.writeString(n.toString)
      case b:Boolean => g.writeBoolean(b)
      case obj : AnyRef =>
        SerializableTypeInfo.get(obj.getClass) match {
        case Some(helper) =>
          g.writeStartObject()
          if (helper.needsTypeTagOnSerialization || alwaysWriteTypeTag) g.writeStringField(typeTag,helper.name)
          for (f<-helper.fields) {
            f.getField(obj) match {
              case null =>
              case None =>
              case a:Array[_] if a.isEmpty && !f.includeEmptyCollections =>
              case a:GenTraversable[_] if a.isEmpty && !f.includeEmptyCollections =>
              case Some(a) =>
                g.writeFieldName(f.name)
                serialize(a,g,alwaysWriteTypeTag)
              case other =>
                g.writeFieldName(f.name)
                serialize(other,g,alwaysWriteTypeTag)
            }
          }
          g.writeEndObject()
        case None =>
          g.writeString(obj.toString)
        }
    }
  }
  
}


