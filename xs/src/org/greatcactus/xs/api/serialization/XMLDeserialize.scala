/**
 * Copyright Andrew Conway 2012-2014. All rights reserved.
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
import org.greatcactus.xs.impl.ValueOfString
import javax.sql.rowset.spi.XmlReader

/**
 * Deserialize an XS object from XML
 */
object XMLDeserialize {

  private[xs] val  inputFactory:XMLInputFactory = XMLInputFactory.newInstance();

  def deserialize[T <: AnyRef : ClassTag](data:Array[Byte]) : T = deserialize(new ByteArrayInputStream(data))

  def deserialize[T <: AnyRef : ClassTag](is:InputStream) : T = {
    try {
      val reader:XMLStreamReader = inputFactory.createXMLStreamReader(is,"UTF-8");
      deserialize(reader)
    } finally { is.close() }
  }
  
  def deserialize[T <: AnyRef : ClassTag](reader:XMLStreamReader) : T = {
    val helper : SerializableTypeInfo[T] = SerializableTypeInfo.get(scala.reflect.classTag[T].runtimeClass).getOrElse(throw new IllegalArgumentException("Cannot deserialize "+scala.reflect.classTag[T].runtimeClass)).asInstanceOf[SerializableTypeInfo[T]]
    while (reader.getEventType()!=XMLStreamConstants.START_ELEMENT) reader.nextTag();
    val name = reader.getLocalName
    if (name!=helper.name) throw new WrongDocumentType(name)
    deserialize(reader,helper,new ListBuffer[EqualityByPointerEquality[AnyRef]])
  }

  /** 
   * This is called when we have just received a START_ELEMENT tag. It finishes when we get an END_ELEMENT tag. 
   **/
  def deserialize[T <: AnyRef](reader:XMLStreamReader,helper:SerializableTypeInfo[T],openNodes:ListBuffer[EqualityByPointerEquality[AnyRef]]) : T = {
    assert (reader.getEventType()==XMLStreamConstants.START_ELEMENT)
    val work = new DeserializeWork(helper)
    work.processAttributes(reader)
    work.processSubtags(reader,openNodes)
  }

  /** 
   * deserialize, merging into a given original object. Extra nodes will be added (if possible) before the given field and index of elements of that field.
   **/
  def deserializeInto[T <: AnyRef](reader:XMLStreamReader,helper:SerializableTypeInfo[T],original:T,loadBefore:Option[(XSFieldInfo,Int)]) : (T,Set[EqualityByPointerEquality[AnyRef]]) = {
    // val helper : SerializableTypeInfo[T] = SerializableTypeInfo.get(scala.reflect.classTag[T].runtimeClass).get.asInstanceOf[SerializableTypeInfo[T]]
    while (reader.getEventType()!=XMLStreamConstants.START_ELEMENT) reader.nextTag();
    if (reader.getLocalName!=XMLSerialize.CopiedDataTag) throw new IllegalArgumentException("Expected "+XMLSerialize.CopiedDataTag+" got "+reader.getLocalName)
    val work = new DeserializeWork(helper)
    work.preload(original,loadBefore)
    work.processAttributes(reader)
    val openNodes = new ListBuffer[EqualityByPointerEquality[AnyRef]]
    val res = work.processSubtags(reader,openNodes)
    (res,openNodes.toSet)
  }

  /**
   * Do the actual work of processing the stuff between a START_ELEMENT tag and an END_ELEMENT tag
   * It works by building up the information for each field as tags are processed, and then when the END_ELEMENT tag
   * is reached, then the actual object is created.
   * This is used in two ways:
   * (1) plain deserialization - the object is constructed, then processAttributes and processSubtags are called.
   * (2) Adding extra data to an existing node - in this case, after the object is created but before the processSubtags is called, the
   *     built up fields are preloaded with the existing data. 
   */
  class DeserializeWork[T <: AnyRef](helper:SerializableTypeInfo[T]) {
    val fields = new Array[AnyRef](helper.numFields) // the contents of the fields, placed in as they are found.
    val buffers = new Array[ArrayBuffer[AnyRef]](helper.numFields) // fields that are arrays or collections are instead built up into here.
    val maphelpers = new Array[MapHelper](helper.numFields)
    var addToBuffersPostDeserialization : Option[Array[Seq[AnyRef]]] = None // Fields that are preloaded from an existing object that should be added to the buffers after the deserialization. Usually not used.
    for (field<-helper.fieldsAsBlocks) if (field.isCollectionOrArray) buffers(field.index) = new ArrayBuffer[AnyRef]
    var hadOpenTag = false
    for (field<-helper.fields; leftOutValue<-field.leftOutValue) fields(field.index)=leftOutValue
    for (field<-helper.mapFields) maphelpers(field.index) = new MapHelper(field,field.mapArgParser,None)
    
    /** Preload esisting data into the structure. Used when extra nodes are being added to an existing node. Extra nodes will be added (if possible) before the given field and index of elements of that field. */
    def preload(existing:T,loadBefore:Option[(XSFieldInfo,Int)]) {
      // deal with attribute fields
      for (f<-helper.fieldsAsAttributes) fields(f.index)=f.getField(existing).asInstanceOf[AnyRef]
      // deal with block fields
      val loadBeforeIndex = loadBefore match {
        case Some((fi,_)) => fi.index
        case None => Integer.MAX_VALUE
      }
      if (loadBefore.isDefined) addToBuffersPostDeserialization=Some(new Array[Seq[AnyRef]](helper.numFields)) 
      //var pastBefore = false
      for (f<-helper.fieldsAsBlocks) {
        //val inBeforeField = loadBefore.map{f eq _._1}.getOrElse(false)
        val fieldValue = f.getField(existing)
        if (f.isCollectionOrArray) {
          val fieldValues : IndexedSeq[AnyRef] = f.getAllFieldElements(existing).map{_.asInstanceOf[AnyRef]}(collection.breakOut)
          if (f.index<loadBeforeIndex) buffers(f.index)++= fieldValues
          else if (f.index==loadBeforeIndex) {
            val cutoff = loadBefore.get._2
            buffers(f.index)++= fieldValues.slice(0,cutoff)
            addToBuffersPostDeserialization.get(f.index)=fieldValues.slice(cutoff,fieldValues.length)
          } else addToBuffersPostDeserialization.get(f.index)=fieldValues
        } else fields(f.index) = f.getField(existing).asInstanceOf[AnyRef]
        //if (inBeforeField) pastBefore = true
      }
    }
    
    def processAttributes(reader:XMLStreamReader) {
      assert (reader.getEventType()==XMLStreamConstants.START_ELEMENT)
      for (field<-helper.fieldsAsAttributes) if (field.isCollectionOrArray) fields(field.index) = field.emptyCollection
      for (i<-0 until reader.getAttributeCount) {
        val name = reader.getAttributeLocalName(i)
        if (name==XMLSerialize.CopiedDataOpenTag) hadOpenTag=true
        else if (!helper.ignoreFieldNamed(name)){
          val field = helper.getAttributeField(name)
          val value = reader.getAttributeValue(i)
          field.parseStringPossiblyMultipleSafe(value) match {
            case Success(parsedValue) => fields(field.index) = parsedValue
            case _ => field.error("Illegal value "+value)
          }
        }
      }
    }
    /** Process the tags, and return the result */
    def processSubtags(reader:XMLStreamReader,openNodes:ListBuffer[EqualityByPointerEquality[AnyRef]]) : T = {
      var inWrapper : Option[XSFieldInfo] = None
      var mapHelper : Option[MapHelper] = None
      while (reader.hasNext) reader.next() match {
        case XMLStreamConstants.START_ELEMENT =>
          val name = reader.getLocalName
          val (field,optsubhelper,isWrapper) = inWrapper match {
            case None => helper.getBlockField(name)
            case Some(field) => (field,field.getBlockField(name),false)
          }
          def readBlockElement(optionalSubHelper:Option[SerializableTypeInfo[_ <: AnyRef]]) = {
            optionalSubHelper match {
              case null =>
                skipToClosingTagAssertingNothingInterestingBefore(reader) // skip ahead to the end element tag for this element.
                null // special tag meaning null element. Yes, I know somewhere in the docs I said (or should have said) that someone who distinguishes None and null is arguably insane. Sorry. Note the qualifier "arguably".
              case Some(subhelper) => // read as element
                deserialize(reader,subhelper,openNodes)
              case None => // read as string
                val contents = getJustStringElementData(reader)
                field.parseStringSingleSafe(contents).getOrElse{field.error("Illegal value "+contents)}
            }
          }
          if (isWrapper) {
             inWrapper = Some(field)    
          } else if (field.isScalaMap) {
            val mh = mapHelper.getOrElse(maphelpers(field.index))
            mh.gotKey(reader)
            if (mh.keyParsers.length==1) {
              if (field.basetypeIsBlock) field.xsinfo match {
                case Some(xsinfo) => mh.gotValue(if (skipToNextOpeningTag(reader)) readBlockElement(field.getBlockField(reader.getLocalName)) else null)
                case None => mh.gotValueString(getJustStringElementData(reader)) // reads up to and including end element.
              } else { mh.gotValueInAttribute(reader); skipToClosingTagAssertingNothingInterestingBefore(reader) }
            } else {
              mapHelper = Some(new MapHelper(field,mh.keyParsers.tail,Some(mh)))
              // next time, go at higher level
            }
          } else {
            val elem = readBlockElement(optsubhelper)
            if (field.isCollectionOrArray) buffers(field.index)+=elem
            else fields(field.index) = elem
          }
        case XMLStreamConstants.END_ELEMENT =>
          if (mapHelper.isDefined) {
            val mh = mapHelper.get
            val newmh = mh.prevMap.get
            newmh.gotValue(mh.mapSoFar)
            mapHelper = if (newmh.prevMap.isDefined) mh.prevMap else None // if the previous map is the general helper, don't use it. 
          } else inWrapper match {
            case None => 
              for (extra<-addToBuffersPostDeserialization;i<-0 until helper.numFields) if (extra(i)!=null) buffers(i)++= extra(i)
              for (field<-helper.fieldsAsBlocks) if (field.isCollectionOrArray && !field.isScalaMap) fields(field.index) = field.collectionOfBuffer(buffers(field.index))
              for (field<-helper.mapFields) fields(field.index) = maphelpers(field.index).mapSoFar
              // don't want default value.
              //for (field<-helper.fields) if (fields(field.index)==null && !field.isCollectionOrArray) fields(field.index)=field.defaultElementValue
              val res = helper.create(fields)
              if (hadOpenTag) openNodes+=new EqualityByPointerEquality(res)
              return res
            case Some(field) => inWrapper=None
          }
        case _ =>
      } 
      helper.deserializeError("XML file stopped in mid read")
    }
    
    /** Help parsing a field that is a scala map */
    class MapHelper(field:XSFieldInfo,val keyParsers:List[ValueOfString],val prevMap:Option[MapHelper]) {
      var mapSoFar : Map[Any,Any] = Map.empty
      var currentKey : Any = _
      def gotValue(value:Any) { mapSoFar+=currentKey->value}
      def gotValueString(s:String) { gotValue(if (s==null) null else field.parser(s)) }
      def gotKey(reader:XMLStreamReader) { currentKey = keyParsers.head(reader.getAttributeValue(null,"key")) }
      def gotValueInAttribute(reader:XMLStreamReader) { gotValueString(reader.getAttributeValue(null,"value")) }
    }

    /** Skip ahead to the end element tag for this element. */
    def skipToClosingTagAssertingNothingInterestingBefore(reader:XMLStreamReader) {
      while (reader.hasNext) reader.next() match {
        case XMLStreamConstants.START_ELEMENT => throw new IllegalArgumentException("Found tag "+reader.getName()+" when not expected")
        case XMLStreamConstants.END_ELEMENT => return
        case _ =>
      }
      throw new IllegalArgumentException("XML reader stopped waiting for end tag")
    }
    /** Skip ahead to the opening tag, returning false if an ending tag is met instead */
    def skipToNextOpeningTag(reader:XMLStreamReader) : Boolean = {
      while (reader.hasNext) reader.next() match {
        case XMLStreamConstants.START_ELEMENT => return true
        case XMLStreamConstants.END_ELEMENT => return false
        case _ =>
      }
      throw new IllegalArgumentException("XML reader stopped waiting for end tag")
    }
  }
  
  private def getJustStringElementData(reader:XMLStreamReader) : String = {
    val res = new StringBuffer()
    while (reader.hasNext()) reader.next() match {
	  case XMLStreamConstants.END_ELEMENT => return res.toString();
	  case XMLStreamConstants.START_ELEMENT => throw new XMLStreamException("Sub element found");
	  case XMLStreamConstants.CHARACTERS => res.append(reader.getText())
    }
	throw new XMLStreamException("No end element found");
  }

  
  
}



class WrongDocumentType(val starttag:String) extends Exception("Unexpected tag "+starttag+" as start of document")
