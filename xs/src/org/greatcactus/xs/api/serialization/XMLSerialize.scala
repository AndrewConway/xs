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

/**
 * Serialize to XML
 */
object XMLSerialize {

  class SerializeInfo(val wantStartDocument:Boolean,val styleSheet:Option[String],val shouldCloseDocument:Boolean)
  val defaultInfo = new SerializeInfo(false,None,true)
  
  val CopiedDataTag = "xs-copied-data"
  val CopiedDataOpenTag = "xs-is-open"
  
  def serializeToByteArray(obj:AnyRef,info:SerializeInfo=defaultInfo) : Array[Byte] = {
    val w = new ByteArrayOutputStream
    serialize(obj,w,info)
    w.toByteArray()
  }
  
  private[xs] val  outputFactory:XMLOutputFactory = XMLOutputFactory.newInstance();
  
  def serialize(obj:AnyRef,out:OutputStream,info:SerializeInfo=defaultInfo)  {
    val writer:XMLStreamWriter = outputFactory.createXMLStreamWriter(out,"UTF-8");
	if (info.wantStartDocument) {
		writer.writeStartDocument("UTF-8","1.0");
		writer.writeCharacters("\n");
		for (style<-info.styleSheet) writer.writeProcessingInstruction("xml-stylesheet", "type=\"text/xsl\" href=\""+style+"\"");
	}
    val helper = SerializableTypeInfo.get(obj.getClass)
    serialize(obj,writer,helper.get,None,Set.empty)
	if (info.shouldCloseDocument) writer.writeEndDocument();
	writer.close();
	out.close()
  }
  
 
  
  def serialize(obj:AnyRef,writer:XMLStreamWriter,info:SerializableTypeInfo[_],nameOverride:Option[String],openInfo:Set[EqualityByPointerEquality[AnyRef]]) {
    val name = nameOverride.getOrElse(info.name)
    val isEmptyElement = info.fieldsAsBlocks.forall{_.isEmpty(obj)}
    if (isEmptyElement) writer.writeEmptyElement(name)
    else writer.writeStartElement(name)
    if (openInfo.contains(new EqualityByPointerEquality(obj))) writer.writeAttribute(CopiedDataOpenTag,"true")
    //println("Class "+info.clazz+" numfields="+info.numFields)
    for (f<-info.fieldsAsAttributes) yield {
      val stringrep = f.getFieldAsString(obj)
      if (stringrep!=null) writer.writeAttribute(f.name,stringrep);
    }
    for (f<-info.fieldsAsBlocks) {
      val field = f.getField(obj)
      if (field!=null) {
        f.wrapperName.map{writer.writeStartElement(_)}
        def print(fieldElement:Any) { 
          if (fieldElement==null) writer.writeEmptyElement(f.nullElementName) 
            // info.error("Contains a collection containing a null"
          else f.xsinfo match {
            case Some(subHelper) =>
              val subinfo = if (subHelper.clazz==fieldElement.getClass) subHelper else SerializableTypeInfo.get(fieldElement.getClass).getOrElse(subHelper.error("Couldn't deal with subclass "+fieldElement.getClass))
              serialize(fieldElement.asInstanceOf[AnyRef],writer,subinfo,f.overridingName,openInfo)
            case None => 
              writer.writeStartElement(f.name)
              writer.writeCharacters(fieldElement.toString)
              writer.writeEndElement();
          }
        }
        field match {
          case m:Map[_,_] => 
            def proc(map:Map[_,_]) {
              for ((key,value)<-map) {
                writer.writeStartElement(f.name)
                writer.writeAttribute("key",key.toString)
                value match {
                  case null =>
                  case mm:Map[_,_] =>
                    proc(mm)
                  case _ if f.basetypeIsBlock =>
                    if (f.xsinfo.isDefined) print(value)
                    else writer.writeCharacters(value.toString)
                  case _ =>
                    writer.writeAttribute("value",value.toString)
                }
                writer.writeEndElement();
              }
            }
            proc(m)
          case a:Array[_] => a.foreach{print _}
          case a:GenTraversable[_] => a.foreach{print _}
          case a:Option[_] => a.foreach{print _}
          case _ => print(field)
        }
        if (f.wrapperName.isDefined) writer.writeEndElement()
      }
    } 
    if (!isEmptyElement) writer.writeEndElement();
    //out.print("</"+name+">")
  }
}


