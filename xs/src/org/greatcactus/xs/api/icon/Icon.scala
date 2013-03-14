/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.icon

import java.io.InputStreamReader
import java.io.BufferedReader
import scala.collection.mutable.ListBuffer
import java.net.URL
import org.greatcactus.xs.util.FileUtil

case class IconRef(val name:String,val classRef:Class[_])

class Icon(val logicalName:String,val elems:List[ConcreteIcon]) {
  /** Get an icon that is most appropriate, taking into account preferred formats and size. */
  def getInstance(formats:Seq[String],size:Option[Int]) : Option[ConcreteIcon] = {
    for (f<-formats) {
      val l= elems.filter{_.mimeType == f}
      //println("Looking for "+f+" found "+l.length+" from "+elems.mkString(";"))
      val sorted = size match {
        case Some(s) => l.sortBy(_.size match { case Some(is)=>(is-s).abs case None => Integer.MAX_VALUE}) 
        case None => l
      }
      if (!sorted.isEmpty) return Some(sorted.head)
    }
    None
  }
  
}

object Icon {
  val html5formats = List("image/svg+xml","image/svg","image/png","image/jpeg","image/gif")
  def ofURL(url:String) = new Icon(url,List(new IconFromURL(url)))
}


trait ConcreteIcon {
  def size : Option[Int] = None
  def mimeType : String
  def url : String
  def data : Array[Byte]
  
}


/** An icon made from a URL. This could be external, and it could be a data: URL. */
class IconFromURL(val url:String) extends ConcreteIcon {
  override def toString = url
  val mimeType = {
    val semicolonPos = url.indexOf(';')
    val lc = url.toLowerCase()
    val res = if (lc.startsWith("data:") && semicolonPos!= -1) lc.substring(5,semicolonPos)
    else if (lc.endsWith(".svg")) "image/svg+xml"
    else if (lc.endsWith(".png")) "image/png"
    else if (lc.endsWith(".jpeg")) "image/jpeg"
    else if (lc.endsWith(".jpg")) "image/jpeg"
    else if (lc.endsWith(".gif")) "image/gif"
    else if (lc.endsWith(".bmp")) "image/bmp"
    else ""
    //println("Found type "+res+" from "+url)
    res
  }
  lazy val data = {
    val connection = (new URL(url)).openConnection()
    val ct = connection.getContentType()
    val data = FileUtil.readAllOfInputStream(connection.getInputStream())
    data
  }
}

object IconFromURL {
  def apply(url:String) = new IconFromURL(url)
}
class ResolvedIcon(val format:String,override val size:Option[Int],val fileName:String,val internalURL:URL) extends ConcreteIcon {
  /** If the client uses this, it should set XSIconManager.urlOfIcon */
  def url:String = IconManifests.urlOfIcon(this)
  
  def mimeType = format match {
    case "svg" => "image/svg+xml"
    case "png" => "image/png"
    case "jpeg" => "image/jpeg"
    case "gif" => "image/gif"
    case f => "image/"+f // probably do a decent job
  }
  
  lazy val contents = ContentsAndDate(internalURL)
  def data = contents.contents 
}

class ContentsAndDate(val contents:Array[Byte],val lastModified:Long) 

object ContentsAndDate {
  def apply(url:URL) = {
    val conn = url.openConnection()
    val data = FileUtil.readAllOfInputStream(conn.getInputStream())
    val lastMod = conn.getLastModified()
    //println("Url "+url+" lastMod "+lastMod)
    new ContentsAndDate(data,lastMod)
  }
}

object ResolvedIcon {
  def formatOfName(name:String) : String = {
    val lastDot = name.lastIndexOf('.')
    if (lastDot== -1) null
    else name.substring(lastDot+1).toLowerCase() match {
      case "jpg" => "jpeg"
      case ext => ext
    }
  }
  
}
