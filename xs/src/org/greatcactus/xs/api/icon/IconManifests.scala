/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.icon

import java.io.InputStreamReader
import java.io.BufferedReader
import scala.collection.mutable.ListBuffer
import java.net.URL

/**
 * Manage icons in XS.
 * 
 * Icons are stored in a directory somewhere on the class path, along with a file xsicons.manifest that describes
 * them and matches them to logical names. There may be multiple versions of such a file. The following lists
 * where is looked (earlier elements searched first)
 * <ul>
 *   <li>In the package the file referring to it is located in</li>
 *   <li>In sub packages of said file</li>
 *   <li>In the /xsicons directory of the classpath</li>
 * </ul>
 *
 */
object IconManifests {

  private var trackedIcons : Map[String,ResolvedIcon] = Map.empty
  private [icon] def addTrackedIcon(ri:ResolvedIcon) {
    synchronized {
      //println("Added tracked icon "+ri.fileName)
      trackedIcons+=ri.fileName->ri
    }
  }

  
  val baseManifest = new IconManifests(List(IconManifest.system))
  private val iconManifestCache = new scala.collection.mutable.HashMap[(ClassLoader,String),IconManifests]
  
  private def getIconManifests(loader:ClassLoader,path:String) = iconManifestCache.getOrElseUpdate((loader,path),getIconManifestsUncached(loader,path))
  
  private def getIconManifestsUncached(loader:ClassLoader,path:String) :IconManifests = {
    //println("Trying to get Icon Manifests for "+path)
    if (path.startsWith("java")) baseManifest else {
      val parent = if (path=="xsicons/") baseManifest
                   else if (path=="") getIconManifests(loader,"xsicons/")
                   else {
                     val dotInd = path.lastIndexOf('/',path.length-2)
                     val newpath = if (dotInd== -1) "" else path.substring(0,dotInd+1)
                     getIconManifests(loader,newpath)     
                   }
      IconManifest(loader,path) match {
        case None => parent
        case Some(manifest) => new IconManifests(manifest::parent.manifests)
      }
    }
  }
  
  /** Get the icon manifests appropriate for a given class */
  def getIconManifests(clazz:Class[_]) : IconManifests = {
    val classname = clazz.getName()
    val lastDot = classname.lastIndexOf('.')
    val name = if (lastDot== -1) "" else classname.substring(0,lastDot+1).replace('.','/')
    val loader = clazz.getClassLoader()
    if (loader==null) baseManifest else getIconManifests(loader,name)
  } 
  
  /** Get the icon corresponding to a given name. Useful for HTTP clients serving icons */
  def iconFromFilename(fileName:String) : Option[ResolvedIcon] = trackedIcons.get(fileName)
    
  /** Should be set by client if used */
  var urlOfIcon : URLOfIcon = null;
  
  
  
  
}

trait URLOfIcon {
  def apply(icon:ResolvedIcon) : String
}

object IconManifest {
  
  // need to cache, as it is possible that multiple classloaders could find the same thing.
  private def cache = new scala.collection.mutable.HashMap[String,Option[IconManifest]]
  
  def apply(loader:ClassLoader,path:String) : Option[IconManifest] = {
    val url = loader.getResource(path+"xsicons.manifest")
    if (url==null) None else cache.getOrElseUpdate(url.toString, try {
      Some(new IconManifest(loader,path,url))
    } catch { case e : Exception => e.printStackTrace(); None })
  }
  
  val system : IconManifest = apply(this.getClass().getClassLoader(),"org/greatcactus/xs/api/icon/lib/").getOrElse(throw new IllegalArgumentException("System icons missing"))
}

object SystemIcons {
  val delete : Icon = IconManifest.system.iconOfLogicalName("Delete")
}

class IconManifest(loader:ClassLoader,path:String,manifest:URL) {

  val icons : List[Icon] = {
//    println("Searching for "+path+"xsicons.manifest in "+loader)
    val r = new BufferedReader(new InputStreamReader(manifest.openStream()))
    //println("Found")
    var line : String = r.readLine()
    var logicalName : String = null
    val iconList = new ListBuffer[Icon]
    val resolvedList = new ListBuffer[ResolvedIcon]
    def finishLogicalName() {
      if (logicalName!=null) {
        iconList+=new Icon(logicalName,resolvedList.toList)
        resolvedList.clear()
        logicalName=null
      }
    } 
    def newicon(name:String,size:Option[Int]) {
      val res = new ResolvedIcon(ResolvedIcon.formatOfName(name),size,name,loader.getResource(path+name))
      resolvedList+=res
      IconManifests.addTrackedIcon(res)
    }
    while (line!=null) {
 //     println("Found line "+line)
      line=line.trim()
      if (line.isEmpty() || line.startsWith("#")) {}
      else if (line.startsWith("[") && line.endsWith("]")) {
        finishLogicalName()
        logicalName = line.substring(1,line.length-1).trim()
      } else {
        val equalsInd = line.indexOf('=')
        if (equalsInd== -1) { // simple file name
          newicon(line,None)
        } else {
          val lhs = line.substring(0,equalsInd).trim()
          val rhs = line.substring(equalsInd+1).trim()
          if (lhs.equalsIgnoreCase("Copyright")) {}
          else {
            newicon(rhs,Some(lhs.toInt))
          }
        }
      }
      line = r.readLine()
    }
    finishLogicalName()
    r.close()
    iconList.toList
  }
  
  val iconOfLogicalName : Map[String,Icon] = Map.empty++(for (icon<-icons) yield icon.logicalName->icon)
}

class IconManifests(val manifests:List[IconManifest]) {
  def iconOfLogicalName(logicalName:String) : Option[Icon] = {
    //println("Getting icon "+logicalName)
    for (m<-manifests;icon<-m.iconOfLogicalName.get(logicalName)) return Some(icon)
    None
  }
}


