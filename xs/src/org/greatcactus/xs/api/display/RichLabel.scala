/**
 * Copyright 2013 Andrew Conway. All rights reserved. 
 */
package org.greatcactus.xs.api.display

import scala.xml.NodeSeq
import java.util.Locale
import scala.xml.XML
import java.text.MessageFormat
import scala.xml.Text

/**
 * A possible result of a @LabelProvider function that can be used as html in a rich client or text in a less rich client.
 */
final class RichLabel(
    /** What to display on a client that supports just text */
    val text:String,
    /** what to display on a client that supports html */
    val html:NodeSeq,
    /** Javascript code that should be run on the client after rendering the above html. Does not work on all clients */
    val postCreationJavascript:List[String]) {
  override def hashCode() = {
    val h1 = if (text==null) 0 else text.hashCode
    val h2 = if (html==null) 0 else html.hashCode
    h1+65519*h2
  }
  override def equals(o:Any) = o match {
    case other:RichLabel => text==other.text && html==other.html && postCreationJavascript==other.postCreationJavascript
    case _ => false
  }
  override def toString = html.toString
  
  def isBlank = text.trim.isEmpty
  def htmlNotBlank = if (isBlank) Text("???") else html
}

/**
 * A possible result of a @LabelProvider function that produces, after localization, the same things that a @LabelProvider can produce.
 */
abstract class Localizable {
  def localize(locale:Locale) : AnyRef
}

/** 
 * A common type of Localizable.
 * It searches through the given sources, looking for localizationKey and taking the first it finds.
 * Once it has found a good source, it also looks for localizationKey.html, and, if it exists, uses that as a source of html (wrapped in a <span> tag).
 * 
 * If args are non-null, then the localized string is assumed to be a MessageFormat, and is processed with the given arguments. 
 * 
 * If all else fails, the localizationKey is returned
 */
class LocalizableFromSources(val sources:List[TextLocalizationSource],val localizationKey:String,args:Array[AnyRef]=null) extends Localizable {
      /** Try looking up with existing key */
  private def lookup(bundle:TextLocalizationResources,locale:Locale) : Option[AnyRef] = for (text1<-bundle.get(localizationKey)) yield {
      def format(msg:String) = if (args==null) msg else {
        val formatter = new MessageFormat(text1);
        formatter.setLocale(locale);
        formatter.format(args)
      } 
      val text = format(text1)  
      bundle.get(localizationKey+".html") match {
        case Some(html) => RichLabel(text,format(html))
        case None => text        
      }
    }
  override def localize(locale:Locale) : AnyRef = {
    for (source<-sources;found<-lookup(source.textResources(locale),locale)) return found;
    return localizationKey
  }
  
}

object RichLabel {

  private val nullLabel = new RichLabel("",scala.xml.Text(""),Nil)
  def apply(text:String,html:NodeSeq,postCreationJavascript:List[String]) = new RichLabel(text,html,postCreationJavascript)
  def apply(text:String,html:NodeSeq) = new RichLabel(text,html,Nil)
  def apply(text:String,html:String) = new RichLabel(text,XML.loadString("<span>"+html+"</span>"),Nil)
  def apply(text:String) = {
    if (text==null) nullLabel 
    else {
      val lines = text.split("\n")
      val html = if (lines.size<2) scala.xml.Text(text)
                 else lines.map{s=> <p>{s}</p>}.toSeq
      new RichLabel(text,html,Nil)
    }
  }
  /** Get a rich label from the result of an dependency injection, or None if there it is null */
  def apply(fromInjection:AnyRef,locale:Locale) : Option[RichLabel] = fromInjection match {
      case null => None
      case s:String => Some(RichLabel(s))
      case s:RichLabel => Some(s)
      case html:NodeSeq => Some(RichLabel(html.text,html))
      case localizable:Localizable => apply(localizable.localize(locale),locale)
      case unknown => throw new IllegalArgumentException("Unknown label type "+unknown.getClass()+" value "+unknown.toString)
  }
  def apply(fromInjection:Option[AnyRef],fromToString: =>String,locale:Locale) : RichLabel = {
    fromInjection.flatMap{apply(_,locale)}.getOrElse(RichLabel(fromToString))
  }
  
}