/**
 * Copyright Andrew Conway 2013-2014. All rights reserved.
 */
package org.greatcactus.xs.api.serialization

import javax.xml.stream._
import scala.reflect.ClassTag
import java.io.InputStream
import javax.xml.stream.events.XMLEvent
import org.greatcactus.xs.impl.SerializableTypeInfo
import scala.collection.mutable.ArrayBuffer
import java.io.ByteArrayInputStream
import org.greatcactus.xs.impl.XSFieldInfo
import org.greatcactus.xs.util.EqualityByPointerEquality
import scala.collection.mutable.ListBuffer
import scala.util.Success
import org.greatcactus.xs.frontend.html.ClientMessage
import java.io.Reader
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.JsonToken
import org.greatcactus.xs.impl.XSFieldInfo
import org.greatcactus.xs.impl.ValueOfString

/**
 * Deserialize an XS object from JSON
 */
object JSONDeserialize {

  
  private[this] def jsonFactory = ClientMessage.jsonFactory
  def deserialize[T <: AnyRef : ClassTag](data:Array[Byte]) : T = deserializeObjAndClose(jsonFactory.createJsonParser(data))
  def deserialize[T <: AnyRef : ClassTag](data:String) : T = deserializeObjAndClose(jsonFactory.createJsonParser(data))
  def deserialize[T <: AnyRef : ClassTag](data:InputStream) : T = deserializeObjAndClose(jsonFactory.createJsonParser(data))
  def deserialize[T <: AnyRef : ClassTag](data:Reader) : T = deserializeObjAndClose(jsonFactory.createJsonParser(data))

  private def deserializeObjAndClose[T <: AnyRef : ClassTag](p:JsonParser) : T = {
    val st = p.nextToken() // should produce START_OBJECT
    if (st!=JsonToken.START_OBJECT) throw new IllegalArgumentException("Expecting start of JSON object")
    val helper : SerializableTypeInfo[T] = SerializableTypeInfo.get(scala.reflect.classTag[T].runtimeClass).getOrElse(throw new IllegalArgumentException("Cannot deserialize "+scala.reflect.classTag[T].runtimeClass)).asInstanceOf[SerializableTypeInfo[T]]
    val res = deserializeObj(p,helper)
    p.close()
    res    
  }
  /** Call when have already got the START_OBJECT token */
  private def deserializeObj[T <: AnyRef](p:JsonParser,helper:SerializableTypeInfo[T]) : T = {
    val info : SerializableTypeInfo[_ <: T] = if (helper.hasSubclasses) {
      val f1t = p.nextToken() // should produce FIELD_NAME
      if (f1t!=JsonToken.FIELD_NAME || p.getCurrentName()!=JSONSerialize.typeTag) throw new IllegalArgumentException("Expecting class name")
      p.nextToken()
      val className = p.getText()
      helper.subClassFromName.getOrElse(className,throw new IllegalArgumentException("Not expecting class "+className+" in class "+helper.name))
    } else helper
    val fields  = new Array[AnyRef](info.numFields) // the contents of the fields, placed in as they are found.
    for (f<-info.fields) fields(f.index)=if (f.isCollectionOrArray) f.emptyCollection else f.defaultElementValue
    while (p.nextToken()!=JsonToken.END_OBJECT) {
      val name = p.getCurrentName()
      if (name!=JSONSerialize.typeTag) {
        val field = info.getField(name)
        fields(field.index)=deserializeField(p,field)    
      }
    }
    // if (fields.length>0) println(fields(0))
    info.create(fields)
  }

  private def deserializeField(p:JsonParser,field:XSFieldInfo) : AnyRef = {
    def proc(token:JsonToken,mapsToDealWith:List[ValueOfString]) : AnyRef = token match {
      case JsonToken.VALUE_NULL => null
      case JsonToken.START_OBJECT =>
        if (field.isScalaMap && !mapsToDealWith.isEmpty) {
          var res : Map[Any,Any] = Map.empty
          while (p.nextToken()!=JsonToken.END_OBJECT) {
            val key = mapsToDealWith.head(p.getCurrentName())
            val value = proc(p.nextToken(),mapsToDealWith.tail)
            res+=key->value
          }
          res
        } else {
          val info : SerializableTypeInfo[AnyRef] = field.xsinfo.getOrElse{throw new IllegalArgumentException("Not expecting object")}.asInstanceOf[SerializableTypeInfo[AnyRef]]
          val res : AnyRef = deserializeObj[AnyRef](p,info)
          res    
        }
      case JsonToken.START_ARRAY =>
        val res = new ArrayBuffer[AnyRef]
        var subt = p.nextToken()
        while (subt!=JsonToken.END_ARRAY) {
          res+=proc(subt,mapsToDealWith)
          subt = p.nextToken()
        }
        field.collectionOfBuffer(res)
      case JsonToken.VALUE_FALSE => java.lang.Boolean.FALSE
      case JsonToken.VALUE_TRUE => java.lang.Boolean.TRUE
      case JsonToken.VALUE_NUMBER_FLOAT => field.baseTypeInfo.className match {
        case "scala.Float" => new java.lang.Float(p.getFloatValue)
        case "scala.Double" => new java.lang.Double(p.getDoubleValue)
        case "java.lang.Float" => new java.lang.Float(p.getFloatValue)
        case "java.lang.Double" => new java.lang.Double(p.getDoubleValue)
        case _ => throw new IllegalArgumentException("Not expecting floating point value for field "+field.name)

      }
      case JsonToken.VALUE_NUMBER_INT => field.baseTypeInfo.className match {
        case "scala.Byte" => new java.lang.Byte(p.getByteValue)
        case "scala.Short" => new java.lang.Short(p.getShortValue)
        case "scala.Int" => new java.lang.Integer(p.getIntValue)
        case "scala.Long" => new java.lang.Long(p.getLongValue)
        case "scala.Float" => new java.lang.Float(p.getFloatValue)
        case "scala.Double" => new java.lang.Double(p.getDoubleValue)
        case "java.lang.Byte" => new java.lang.Byte(p.getByteValue)
        case "java.lang.Short" => new java.lang.Short(p.getShortValue)
        case "java.lang.Integer" => new java.lang.Integer(p.getIntValue)
        case "java.lang.Long" => new java.lang.Long(p.getLongValue)
        case "java.lang.Float" => new java.lang.Float(p.getFloatValue)
        case "java.lang.Double" => new java.lang.Double(p.getDoubleValue)
        case _ => throw new IllegalArgumentException("Not expecting integer for field "+field.name+" type "+field.baseTypeInfo.className)
      }
      case JsonToken.VALUE_STRING => field.parseStringSingle(p.getText())
    }
    val raw = proc(p.nextToken(),field.mapArgParser)
    if (field.isScalaOption) Some(raw) else raw
  }
        
  
}

