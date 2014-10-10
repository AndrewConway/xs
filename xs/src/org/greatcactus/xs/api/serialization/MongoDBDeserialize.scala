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
import org.bson.BSONObject
import org.greatcactus.xs.impl.ValueOfString

/**
 * Deserialize an XS object from JSON
 */
object MongoDBDeserialize {

  def deserialize[T <: AnyRef : ClassTag](p:BSONObject) : T = {
    val helper : SerializableTypeInfo[T] = SerializableTypeInfo.get(scala.reflect.classTag[T].runtimeClass).getOrElse(throw new IllegalArgumentException("Cannot deserialize "+scala.reflect.classTag[T].runtimeClass)).asInstanceOf[SerializableTypeInfo[T]]
    val res = deserializeObj(p,helper)
    res    
  }
  /** Call when have already got the START_OBJECT token */
  private def deserializeObj[T <: AnyRef](p:BSONObject,helper:SerializableTypeInfo[T]) : T = {
    p.get(JSONSerialize.typeTag) match {
      case className:String =>
        val info : SerializableTypeInfo[_ <: T] = helper.subClassFromName.getOrElse(className,throw new IllegalArgumentException("Not expecting class "+className+" in class "+helper.name))
        val fields  = new Array[AnyRef](info.numFields) // the contents of the fields, placed in as they are found.
        for (f<-info.fields) fields(f.index)= p.get(f.name) match {
          case null => if (f.isCollectionOrArray) f.emptyCollection else f.defaultElementValue
          case fvalue => deserializeField(fvalue,f)
        }
        info.create(fields)
      case _ => throw new IllegalArgumentException("Expecting class name")
    }
  }
  private def deserializeField(value:AnyRef,field:XSFieldInfo) : AnyRef = {
    def parseBase(bo:Any) : AnyRef = bo match {
      case null => null
      case bson:BSONObject => 
         val info : SerializableTypeInfo[AnyRef] = field.xsinfo.getOrElse{throw new IllegalArgumentException("Not expecting object")}.asInstanceOf[SerializableTypeInfo[AnyRef]]
         deserializeObj(bson,info)
//      case v if field.isValidBaseType(v) => v  // Optimization
      case s => field.parseStringSingle(s.toString)
    }
    if (value==null) null
    else if (field.isScalaMap) {
      def parse(togo:List[ValueOfString],bo:Any) : AnyRef = {
        togo match {
          case Nil => parseBase(bo)
          case h::t => bo match {
            case null => null
            case bson:BSONObject =>
              var map : Map[Any,Any] = Map.empty
              import scala.collection.JavaConverters._
              for (key<-bson.keySet().asScala) map+=h(key)->parse(t,bson.get(key))
              map
            case _ => throw new IllegalArgumentException("Expecting map")
          } 
        }
      }
      parse(field.mapArgParser,value)
    } else if (field.isCollectionOrArrayButNotOption) {
      value match {
        case a:Array[_] => field.collectionOfBuffer(a.map{parseBase _})
        case _ => throw new IllegalArgumentException("Expecting array")
      }
    } else if (field.isScalaOption) Some(parseBase(value))
    else parseBase(value)
  }
        
  
}

