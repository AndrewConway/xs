/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.api.display

import java.util.ResourceBundle
import java.util.Locale
import java.util.MissingResourceException
import scala.collection.mutable.ListBuffer
import java.text.MessageFormat

/**
 * For each class, provides the text localization information for it.
 */

trait TextLocalizationResources {
  /** The main API. Get (if present) the resource corresponding to a given key. */
  def locale:Locale
  def get(key:String) : Option[String]
  def missing(key:String)
  def apply(key:String) : String = get(key).getOrElse{missing(key); key}
  
  def format(key:String,args:Any*) : String = {
    val formatter = new MessageFormat(apply(key))
    formatter.setLocale(locale)
    formatter.format(args.toArray)
  }
}

class TextLocalizationResourcesAsSeq(files:Seq[TextLocalizationSingleFile],val locale:Locale) extends TextLocalizationResources {

  def get(key:String) : Option[String] = {
    for (f<-files;v<-f.get(key)) return Some(v)
    None
  }
  
  def missing(key:String) {
    if (!files.isEmpty) println("Missing text resource "+key+" for "+files.head.name)
  }
}

class PrefixedTextLocalizationResources(base:TextLocalizationResources,prefix:String) extends TextLocalizationResources {
  override def locale = base.locale
  override def get(key:String) = base.get(prefix+key)
  override def missing(key:String) = base.missing(key)
}

object TextLocalizationResources {
  private val cache = new collection.mutable.HashMap[(Locale,Class[_]),TextLocalizationResources]
  def getCached(locale:Locale,clazz:Class[_]) = cache.getOrElseUpdate((locale,clazz),getUncached(locale,clazz))
  
  def getUncached(locale:Locale,clazz:Class[_]) = {
    val res = new ListBuffer[TextLocalizationSingleFile]
    var c = clazz
    def checkInterfaces(c:Class[_]) {
      for (i<-c.getInterfaces()) if (!i.getCanonicalName().startsWith("java") && !i.getCanonicalName().startsWith("scala")) {
        for (r<-getSingleFileUncached(locale,i,false)) res+=r
        checkInterfaces(i)
      }
    }
    while (c!=null && !c.getCanonicalName().startsWith("java") && !c.getCanonicalName().startsWith("scala")) {
      for (r<-getSingleFileUncached(locale,c,c==clazz)) res+=r
      checkInterfaces(c)
      c=c.getSuperclass
    }
    // also look at interfaces
    new TextLocalizationResourcesAsSeq(res.toList,locale)
  }
  private def getTopLevelClass(clazz:Class[_]) : Class[_] = {
    val cc = clazz.getDeclaringClass()
    if (cc==null) clazz else getTopLevelClass(cc)
  }
  private def getSingleFileUncached(locale:Locale,clazz:Class[_],required:Boolean) : Option[TextLocalizationSingleFile] = {
    val resourceBundleName = getTopLevelClass(clazz).getCanonicalName();
    try {
      val rb = ResourceBundle.getBundle(resourceBundleName,locale, clazz.getClassLoader())
      val name = clazz.getCanonicalName()
      val prefix = if (name==resourceBundleName) "" else name.substring((resourceBundleName.length+1) min name.length)+"."
      Some(new TextLocalizationSingleFile(prefix,rb,name))
    } catch { 
      case _:MissingResourceException => 
        if (required) System.err.println("Missing resource bundle "+resourceBundleName)
        None
    }
  }
  
  
}

class TextLocalizationSingleFile(val prefix:String,val bundle:ResourceBundle,val name:String) {
  def get(key:String) : Option[String] = {
    try {
      Some(bundle.getString(prefix+key))
    } catch { case _:MissingResourceException => None}
  } 
  
  def missingError(key:String) = {
    (new IllegalArgumentException()).printStackTrace();
	System.err.println("Expecting resource "+prefix+key+" in "+name);
  }
}

trait TextLocalizationSource {
  def textResources(locale:Locale) : TextLocalizationResources
}