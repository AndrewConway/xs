/**
 * Copyright Andrew Conway 2013. All rights reserved.
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
import java.net.URLDecoder

/**
 * Deserialize an XS object from a string map. Complement of StringMapSerialize.
 */
object StringMapDeserialize {

  private val SplitEquals="""([^=]*)=(.*)""".r
  
  def deserializeURL[T <: AnyRef : ClassTag](urlQueryString:String) : T = {
    val map = Map(urlQueryString.split("&").collect{case SplitEquals(lhs,rhs) =>lhs->URLDecoder.decode(rhs,"UTF-8")} :_*)
    deserialize[T](map)
  }

  def deserialize[T <: AnyRef : ClassTag](data:Map[String,String]) : T = {
    val helper : SerializableTypeInfo[T] = SerializableTypeInfo.get(scala.reflect.classTag[T].runtimeClass).getOrElse(throw new IllegalArgumentException("Cannot deserialize "+scala.reflect.classTag[T].runtimeClass)).asInstanceOf[SerializableTypeInfo[T]]
    deserializeObj(data,helper,"")
  }

  private def deserializeObj[T <: AnyRef](data:Map[String,String],helper:SerializableTypeInfo[T],prefix:String) : T = {
    data.get(prefix+StringMapSerialize.typeTag) match {
      case Some(className) =>
        val info : SerializableTypeInfo[_ <: T] = helper.subClassFromName.getOrElse(className,throw new IllegalArgumentException("Not expecting class "+className+" in class "+helper.name))
        val fields  = new Array[AnyRef](info.numFields) // the contents of the fields, placed in as they are found.
        for (f<-info.fields) fields(f.index)=deserializeField(data,f,prefix+f.name)
        info.create(fields)
      case None => null.asInstanceOf[T]
    }
  }
  private def deserializeField(data:Map[String,String],field:XSFieldInfo,prefix:String) : AnyRef = {
    def elem(elemPrefix:String) : AnyRef = {
      field.xsinfo match {
        case Some(helper) => deserializeObj(data,helper.asInstanceOf[SerializableTypeInfo[AnyRef]],elemPrefix+"_")
        case None =>
          data.get(elemPrefix).map{field.parseStringSingle(_)}.getOrElse(null)
      }
    }
    if (field.isCollectionOrArrayButNotOption) {
      val res = new ArrayBuffer[AnyRef]
      val length = data.get(prefix+"_len").getOrElse("0").toInt
      for (i<-0 until length) res+=elem(prefix+"_"+i)
      field.collectionOfBuffer(res)
    } else if (field.isScalaOption) Option(elem(prefix))
    else {
      val res = elem(prefix)
      if (res==null) field.defaultElementValue else res
    }
  }
        
  
}

