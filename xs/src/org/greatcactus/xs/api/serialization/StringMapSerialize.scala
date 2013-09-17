/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.api.serialization

import org.greatcactus.xs.impl.SerializableTypeInfo
import scala.collection.GenTraversable
import org.greatcactus.xs.impl.CollectionStringUtil
import java.net.URLEncoder

/**
 * Serialize to a String=>String map. This is designed for small, simple structures that can have the result displayed as a URL.
 * 
 * A class is serialized with entries for all its non-empty fields, plus one "xsType" with the name of the class being serialized.
 * 
 * Fields are serialized as follows:
 *   - non-XS fields are serialized via toString
 *   - non-empty collections called "x" are serialized as x_len=number, x_0, x_1, x_2, etc
 *   - non-null object fields called "x" , or non-empty options, are serialized objects prefixed by "x_" 
 *  Polymorphism is dealt with by the xsType field.
 *  0, false, empty string, and null are not written out.
 */
object StringMapSerialize {

  val typeTag = "type" // field for types. 
    
  /** Serialize to a URL query string (the thing after a "?" in a URL) */
  def toURL(toSerialize:Any) : String = {
    val map = serialize(toSerialize)
    map.map{case (key,value)=>key+"="+URLEncoder.encode(value,"UTF-8")}.mkString("&")
  }
    
  /** Serialize to a map */
  def serialize(toSerialize:Any,prefix:String="") : Map[String,String] = {
    toSerialize match {
      case null => Map.empty
      case a:Array[_] => 
        var res = Map(prefix+"_len"->a.length.toString)
        for (i<-0 until a.length) res++=serialize(a(i),prefix+"_"+i)
        res
      case a:GenTraversable[_] => 
        var res = Map(prefix+"_len"->a.size.toString)
        for ((ai,i)<-a.toList.zipWithIndex) res++=serialize(ai,prefix+"_"+i)
        res
      case 0 => Map.empty
      case n:Int => Map(prefix->n.toString)
      case n:Short => Map(prefix->n.toString)
      case n:Double => Map(prefix->n.toString)
      case n:Float => Map(prefix->n.toString)
      case n:Byte => Map(prefix->n.toString)
      case n:Char => Map(prefix->n.toString)
      case false => Map.empty
      case b:Boolean => Map(prefix->b.toString)
      case s:String if s.isEmpty => Map.empty
      case obj : AnyRef =>
        SerializableTypeInfo.get(obj.getClass) match {
        case Some(helper) =>
          val modprefix = if (prefix.isEmpty) prefix else prefix+"_"
          var res = Map(modprefix+typeTag->helper.name)
          for (f<-helper.fields) {
            f.getField(obj) match {
              case null =>
              case None =>
              case a:Array[_] if a.isEmpty =>
              case a:GenTraversable[_] if a.isEmpty =>
              case Some(a) => res++=serialize(a,modprefix+f.name)
              case other => res++=serialize(other,modprefix+f.name)
            }
          }
          res
        case None => 
          val s = obj.toString
          if (s.isEmpty) Map.empty else Map(prefix->obj.toString)
        }
    }
  }
  
}


