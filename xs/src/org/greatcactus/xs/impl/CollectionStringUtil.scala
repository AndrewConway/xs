/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.impl

import scala.collection.GenTraversable
import scala.collection.mutable.ArrayBuffer

/**
 * Utilities associated with representation of collections as semicolon separated strings.
 * Escaping is done on semicolon separated strings : ";" and "`" are escaped by preceding
 * them by "`". 
 */
object CollectionStringUtil {

    /**
	 * Like res.append(s), except escape ; and ` characters. In particular,
	 * ` is saved as ``
	 * ; is saved as `;
	 * @param res where to append the string
	 * @param s the string to escape and append.
	 */
  def appendWithSemicolonsEscaped(res:StringBuilder,s:String) {
    for (c<-s) {
      if (c=='`' || c==';') res.append('`');
	  res.append(c);
    }
  }
  
  def joinSemicolonListEscaped(collection:GenTraversable[Any]) : String = {
    if (collection.isEmpty) null else {
      val res = new StringBuilder
      var hadFirst = false
      for (v<-collection) {
        if (hadFirst) res.append(';') else hadFirst=true
        if (v!=null) appendWithSemicolonsEscaped(res,v.toString)
      }
      res.toString
    }
  }
  def joinSemicolonListEscaped(collection:Array[Any]) : String = {
    if (collection.length==0) null else {
      val res = new StringBuilder
      var hadFirst = false
      for (v<-collection) {
        if (hadFirst) res.append(';') else hadFirst=true
        if (v!=null) appendWithSemicolonsEscaped(res,v.toString)
      }
      res.toString
    }
  }
  
  private val empty = new Array[String](0)
  
      
	/**
	 * Separate a semicolon separated list of strings escaped using {@link #escapeSemicolon(StringBuilder, String)}.
	 * @param tosplit The string to split.
	 * @param decodeEscapes Whether you want to decode the escape characters at the same time (almost always true).
	 * @return null iff tosplit is null.
	 */
  
  def separateSemicolonListEscaped(tosplit:String,decodeEscapes:Boolean=true) : Array[String] = {
    if (tosplit==null) empty else {
      val res = new ArrayBuffer[String]
      var escaped = false
      var str = new StringBuilder
      def endstring() { res+=str.toString; str.clear() }
      for (c<-tosplit) {
        if (escaped) { str.append(c); escaped=false }
        else if (c=='`') { escaped=true; if (!decodeEscapes) str.append(c) }
        else if (c==';') endstring()
        else str.append(c)
      }
      endstring()
      res.toArray
    }
  }
}

trait LengthInfo {
  /** The number of fields this works for */
  def numFields : Int
  /** The location that the given field starts at */
  def startField(elem:Int) : Int
  /** The location that the given field ends at */
  def endField(elem:Int) : Int
  /** The location of the posWithinElem'th field of the elem'th element */
  def relativePosition(elem:Int,posWithinElem:Int) : Int
}

/** Information on how the human entered value (e.g <space><space>010.000<space> matches against the canonical value 10.0 which is generally the one used for error messages */ 
class TrimInfo(val trimmedFromStart:Int,val trimmedLength:Int,val totalLength:Int) {
  override def toString=""+trimmedFromStart
}
object TrimInfo {
  def apply(s:String) : TrimInfo = {
    val trimmed = s.trim()
    val start = s.indexOf(trimmed)
    new TrimInfo(start,trimmed.length,s.length)
  }
  val empty = Array[Option[TrimInfo]]()
}
/** UTility for converting from indices in a collection of strings to indices in the string returned from joinSemicolonListEscaped */
class CollectionLengthInfo(strings:Array[String],humanEditedInfo:Array[Option[TrimInfo]]) extends LengthInfo {
  val numIndices : Int = strings.length
  val cumulativeLengths = new Array[Int](numIndices+1)
  val escapePositions : Array[Seq[Int]] = for (s<-strings) yield (0 until s.length).filter{i => val c=s(i); c=='`'||c==';'}
  def humanEdited(elem:Int) = if (humanEditedInfo.length>elem) humanEditedInfo(elem) else None
  for (i<-0 until numIndices) cumulativeLengths(i+1)=cumulativeLengths(i)+1+humanEdited(i).map{_.totalLength}.getOrElse(strings(i).length+escapePositions(i).length)
  def cumulativeLength(index:Int) : Int = cumulativeLengths(index)
  def numFields : Int = numIndices
  def startField(elem:Int) : Int = cumulativeLength(elem)+humanEdited(elem).map{_.trimmedFromStart}.getOrElse(0)
  def endField(elem:Int) : Int = humanEdited(elem) match {
    case None => cumulativeLength(elem+1)-1
    case Some(ti) => cumulativeLength(elem)+ti.trimmedFromStart+ti.trimmedLength
  }
  def relativePosition(elem:Int,posWithinElem:Int) : Int = cumulativeLengths(elem)+escapePositions(elem).filter{_<posWithinElem}.length+(humanEdited(elem) match {
      case None => posWithinElem
      case Some(ti) => ti.trimmedFromStart+posWithinElem.min(ti.trimmedLength)
  })
}

class SingleLengthInfo(string:String,humanEdited:Option[TrimInfo]) extends LengthInfo {
  val length = string.length
  def numFields = 1
  def startField(elem:Int) = humanEdited match {
    case None => 0
    case Some(ti) => ti.trimmedFromStart
  }
  def endField(elem:Int) = humanEdited match {
    case None => string.length
    case Some(ti) => ti.trimmedFromStart+ti.trimmedLength
  }
  def relativePosition(elem:Int,posWithinElem:Int) : Int = humanEdited match {
    case None => posWithinElem
    case Some(ti) => ti.trimmedFromStart+posWithinElem.min(ti.trimmedLength)    
  }
}
