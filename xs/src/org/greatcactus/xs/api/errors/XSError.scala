/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.api.errors

import org.greatcactus.xs.api.display.RichLabel
import java.util.Locale
import org.greatcactus.xs.impl.CollectionLengthInfo
import org.greatcactus.xs.impl.SingleLengthInfo
import org.greatcactus.xs.impl.LengthInfo

/**
 * An error to be reported by XS
 */
class XSError(
    /** How severe the error is */
    val severity:Severity,
    /** Text describing the error. This can be a string, a RichLabel, or a Localizable */
    val description:AnyRef,
    /** The first (inclusive) character that this refers to, or -1 */
    val from:Int,
    /** The last (exclusive) character that this refers to, or -1 */
    val to:Int,
    /** The element in the collection this refers to, 0 if not a collection, or -1 if not applicable */
    val collectionIndex:Int
    ) {
  
  override def hashCode = severity.hashCode*65521+(if (description==null) 0 else description.hashCode()*31)+from+to+collectionIndex
  
  override def equals(obj:Any) = obj match {
    case other : XSError => severity==other.severity && description==other.description && from==other.from && to==other.to && collectionIndex==other.collectionIndex
    case _ => false
  }
  
  def hasLocation : Boolean = from!= -1 && to!= -1
  
  def charLength : Int = to-from
 
  def resolve(locale:Locale,collectionLengths:Option[LengthInfo]) = {
    val desc = RichLabel(description,locale).getOrElse(null)
    collectionLengths match {
      case Some(lens:LengthInfo) if collectionIndex>=0 && collectionIndex<lens.numFields => 
        val newfrom = if (from== -1) lens.startField(collectionIndex) else lens.relativePosition(collectionIndex,from)
        val newto = if (to== -1) lens.endField(collectionIndex) else lens.relativePosition(collectionIndex,to)
        new ResolvedXSError(severity,desc,newfrom,newto)
      case _ => new ResolvedXSError(severity,desc,from,to)
    }
  }

}


object XSError {
  def apply(severity:Severity,description:AnyRef,from:Int,to:Int,collectionIndex:Int) = new XSError(severity,description,from,to,collectionIndex)
  def apply(severity:Severity,description:AnyRef,from:Int,to:Int) = new XSError(severity,description,from,to,0)
  def apply(severity:Severity,description:AnyRef) = new XSError(severity,description,-1,-1,0)
  def apply(severity:Severity,description:AnyRef,collectionIndex:Int) = new XSError(severity,description,-1,-1,collectionIndex)
  def error(description:AnyRef,from:Int,to:Int,collectionIndex:Int) : XSError = new XSError(Severity.ERROR,description,from,to,collectionIndex)
  def error(description:AnyRef,from:Int,to:Int) : XSError = new XSError(Severity.ERROR,description,from,to,0)
  def error(description:AnyRef,collectionIndex:Int) : XSError = new XSError(Severity.ERROR,description,-1,-1,collectionIndex)
  def error(description:AnyRef) : XSError = new XSError(Severity.ERROR,description,-1,-1,0)
  def warning(description:AnyRef,from:Int,to:Int,collectionIndex:Int) : XSError = new XSError(Severity.WARNING,description,from,to,collectionIndex)
  def warning(description:AnyRef,from:Int,to:Int) : XSError = new XSError(Severity.WARNING,description,from,to,0)
  def warning(description:AnyRef,collectionIndex:Int) : XSError = new XSError(Severity.WARNING,description,-1,-1,collectionIndex)
  def warning(description:AnyRef) : XSError = new XSError(Severity.WARNING,description,-1,-1,0)
}


/** Like XSError, except with description resolved, and collectionIndex dealt with by modifying from and to to be with respect to the semicolon joined string representation of the collection. */
class ResolvedXSError(val severity:Severity,val description:RichLabel,val from:Int,val to:Int) {
  override def hashCode = severity.hashCode*65521+(if (description==null) 0 else description.hashCode()*31)+from+to
  
  override def equals(obj:Any) = obj match {
    case other : ResolvedXSError => severity==other.severity && description==other.description && from==other.from && to==other.to
    case _ => false
  }  
}

