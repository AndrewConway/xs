/**
 * Copyright Andrew Conway 2013-2014. All rights reserved.
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
import com.mongodb.BasicDBObject
import com.mongodb.BasicDBObjectBuilder
import com.mongodb.DBObject

/**
 * Serialize to MongoDB object. Structure is same as JSON serialization.
 */
object MongoDBSerialize {

  def typeTag = JSONSerialize.typeTag // field for types. 
  
  def serializeDBO(obj:Any) : DBObject = serialize(obj).asInstanceOf[DBObject]

  def serialize(toSerialize:Any) : AnyRef = {
    toSerialize match {
      case null => null
      case a:Array[_] => a.map{serialize(_)}
      case m:Map[_,_] =>
          val builder = BasicDBObjectBuilder.start()
          for ((key,value)<-m) {
            builder.add(key.toString,serialize(value))
          }
          builder.get()
      case a:GenTraversable[_] => a.map{serialize(_)}.toArray 
      case n:Int => new java.lang.Integer(n)
      case n:Short => new java.lang.Short(n)
      case n:Double => new java.lang.Double(n)
      case n:Float => new java.lang.Float(n)
      case n:Byte => new java.lang.Byte(n)
      case n:Char => new java.lang.Character(n)
      case b:Boolean => java.lang.Boolean.valueOf(b)
      case obj : AnyRef => SerializableTypeInfo.get(obj.getClass) match {
        case Some(helper) =>
          val builder = BasicDBObjectBuilder.start()
          if (helper.needsTypeTagOnSerialization) builder.add(typeTag,helper.name)
          for (f<-helper.fields) {
            f.getField(obj) match {
              case null =>
              case None =>
              case a:Array[_] if a.isEmpty =>
              case a:GenTraversable[_] if a.isEmpty =>
              case Some(a) =>
                builder.add(f.name,serialize(a))
              case other =>
                builder.add(f.name,serialize(other))
            }
          }
          builder.get()
        case None =>
          obj.toString
        }
    }
  }
  
}


