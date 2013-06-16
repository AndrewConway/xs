/**
 * Copyright 2013 Andrew Conway. All rights reserved
 */
package org.greatcactus.xs.impl

import org.greatcactus.xs.frontend.XSTreeNode
import org.greatcactus.xs.api.errors.XSError
import scala.collection.GenTraversable
import org.greatcactus.xs.api.errors.Severity
import scala.util.matching.Regex
import java.lang.NumberFormatException
import scala.collection.mutable.ListBuffer
import java.util.Locale
import org.greatcactus.xs.api.display._
import java.text.MessageFormat

/**
 * Deal with simple error checks (like @ErrorIfNull)
 *
 */
class SimpleErrorChecks(val checks:Map[String,List[SimpleErrorCheck]]) {
  def isEmpty = checks.isEmpty
  def check(parent:XSTreeNode) = new SimpleErrorCheckResults(Map.empty++(
    for ((field,checklist)<-checks;errors=checklist.flatMap{_(parent)};if !errors.isEmpty) yield field->errors  
  ))
}

class SimpleErrorCheckResults(val errors:Map[String,List[XSError]])

class SimpleErrorChecksBuffer {
  var map : Map[String,List[SimpleErrorCheck]] = Map.empty
  def add(fieldName:String,check:SimpleErrorCheck) {
    val existing = map.get(fieldName).getOrElse(Nil) 
    //println("Adding Simple Error Check for "+fieldName+" type "+check.getClass.getName)
    map+=fieldName->(check::existing)
  }
  private def sev(severity:Option[String]) = severity.getOrElse("ERROR").toUpperCase match {
    case "ERROR" => Severity.ERROR
    case "WARNING" => Severity.WARNING
    case "INFO" => Severity.INFO
    case _ => throw new IllegalArgumentException("Unrecognised error severity "+severity)
  } 
  def addErrorIfBlank(field:XSFieldInfo,severity:Option[String]) { add(field.name,new PseudoDIErrorIfBlank(field,sev(severity)))} 
  def addErrorIfEmptyCollection(field:XSFieldInfo,severity:Option[String]) { add(field.name,new PseudoDIErrorIfEmptyCollection(field,sev(severity)))} 
  def addErrorIfNotSuffix(field:XSFieldInfo,suffix:String,severity:Option[String]) { add(field.name,new PseudoDIErrorIfNotSuffix(field,suffix,sev(severity)))} 
  def addErrorIfNotRegex(field:XSFieldInfo,regexp:String,severity:Option[String]) { add(field.name,new PseudoDIErrorIfNotRegex(field,regexp.r,sev(severity)))} 
  def addErrorIfNotUnique(field:XSFieldInfo,key:UniquenessClass,getBadness:XSTreeNode=>NonuniqueElements,severity:Option[String]) { add(field.name,new PseudoDIErrorIfNotUnique(field,key,getBadness,sev(severity)))} 
  def addErrorIfNotNumber(field:XSFieldInfo,integer:Boolean,min:Double,max:Double,severity:Option[String]) { add(field.name,new PseudoDIErrorIfNotNumber(field,integer,min,max,sev(severity)))}
  def addErrorIfNegative(field:XSFieldInfo,severity:Option[String]) { add(field.name,new PseudoDIErrorIfNegative(field,sev(severity)))}
  def get = new SimpleErrorChecks(map)
}
/**
 * All the error annotations have implementations that subclass this
 */
abstract class SimpleErrorCheck {
  def field:XSFieldInfo
  def severity : Severity
  def localizationKey : String
  def localizationArgs : Array[AnyRef] = null
  val errorDescription = new LocalizableFromSources(List(field,SimpleErrorMessageSource),localizationKey,localizationArgs)
  def trivialError = XSError(severity,errorDescription,-1,-1,-1)
  def error(fieldNo:Int) = XSError(severity,errorDescription,-1,-1,fieldNo)

  def apply(parent:XSTreeNode) : List[XSError] = {
    val res = new ListBuffer[XSError]
    apply(parent,field.getAllFieldElements(parent.getObject),res)
    //println("Testing "+parent+" for "+this.getClass.getName()+" value "+field.getFieldAsString(parent.getObject)+" result "+res.toList.mkString(";"))
    res.toList
  }
  def apply(parent:XSTreeNode,elems:GenTraversable[Any],res:ListBuffer[XSError]) {
    for ((elem,index)<-elems.toList.zipWithIndex) if (!isElemOK(elem)) res+=error(index)
  }
  def isElemOK(elem:Any) : Boolean
}


class PseudoDIErrorIfBlank(val field:XSFieldInfo,val severity:Severity) extends SimpleErrorCheck {
  def localizationKey = "ErrorIfBlank"
  override def apply(parent:XSTreeNode,elems:GenTraversable[Any],res:ListBuffer[XSError]) {
    if (elems.isEmpty && !field.isCollectionOrArrayButNotOption) res+=error(-1)
    super.apply(parent,elems,res)
  }
  def isElemOK(elem:Any) = elem!=null && elem.toString!=""
}

class PseudoDIErrorIfEmptyCollection(val field:XSFieldInfo,val severity:Severity) extends SimpleErrorCheck {
  def localizationKey = "ErrorIfEmptyCollection"
  override def apply(parent:XSTreeNode,elems:GenTraversable[Any],res:ListBuffer[XSError]) {
    if (elems.isEmpty) res+=error(-1)
  }
  def isElemOK(elem:Any) = true
}

class PseudoDIErrorIfNotUnique(val field:XSFieldInfo,val key:UniquenessClass,val getBadness:XSTreeNode=>NonuniqueElements,val severity:Severity) extends SimpleErrorCheck {
  def localizationKey = "ErrorIfNotUnique"
  override def apply(parent:XSTreeNode,elems:GenTraversable[Any],res:ListBuffer[XSError]) {
    for (bads <- getBadness(parent).map.get(key))
      for ((elem,index)<-elems.toList.zipWithIndex) if (bads!=null && bads.contains(elem.asInstanceOf[AnyRef])) res+=error(index)
  }
  override def isElemOK(elem:Any) = true // NOT USED
}

trait SimpleTextErrorCheck extends SimpleErrorCheck {
  def isElemOK(elem:Any) = elem==null || { val s=elem.toString; s.isEmpty || isOK(s)}
  def isOK(s:String) : Boolean
}


class PseudoDIErrorIfNotSuffix(val field:XSFieldInfo,val suffix:String,val severity:Severity) extends SimpleTextErrorCheck {
  def localizationKey = "ErrorIfNotSuffix"
  override def localizationArgs = Array(suffix)
  def isOK(s:String) = s.endsWith(suffix)
}

class PseudoDIErrorIfNotRegex(val field:XSFieldInfo,val regex:Regex,val severity:Severity) extends SimpleTextErrorCheck {
  def localizationKey = "ErrorIfNotRegex"
  override def localizationArgs = Array(regex)
  def isOK(s:String) = regex.pattern.matcher(s).matches
}

class PseudoDIErrorIfNotNumber(val field:XSFieldInfo,val integer:Boolean,val min:Double,val max:Double,val severity:Severity) extends SimpleTextErrorCheck {
  def localizationKey = if (integer) "ErrorIfNotInteger" else "ErrorIfNotNumber"
  def isOK(s:String) : Boolean = {
    try {
      val num = s.toDouble
      if (integer && (num!=num.toInt.toDouble)) false
      else ! (min>num || max<num ) // done with the inversion such that Double.NaN works correctly.  
    } catch { case _:NumberFormatException => false }
  }
}

class PseudoDIErrorIfNegative(val field:XSFieldInfo,val severity:Severity) extends SimpleTextErrorCheck {
  def localizationKey = "ErrorIfNegative"
  def isOK(s:String) : Boolean = {
    try {
      s.toDouble >= 0
    } catch { case _:NumberFormatException => false }
  }
}
     

object SimpleErrorMessageSource extends TextLocalizationSource {
  def textResources(locale:Locale) = TextLocalizationResources.getCached(locale,classOf[SimpleErrorChecks]) 
}



