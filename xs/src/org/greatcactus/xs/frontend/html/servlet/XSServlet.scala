/**
 * Copyright 2012-2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.frontend.html.servlet

import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.greatcactus.xs.frontend.XSEdit
import java.util.Locale
import org.greatcactus.xs.frontend.html._
import scala.collection.mutable.Queue
import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedBlockingQueue
import org.greatcactus.xs.api.serialization.XMLDeserialize
import java.io.FileInputStream
import java.io.File
import org.greatcactus.xs.api.serialization.XMLSerialize
import java.io.FileOutputStream
import org.greatcactus.xs.api.icon.IconManifests
import org.greatcactus.xs.api.icon.URLOfIcon
import org.greatcactus.xs.api.icon.ResolvedIcon
import java.net.URLEncoder
import scala.concurrent.Await
import org.greatcactus.xs.frontend.XSToolBar
import scala.concurrent.ExecutionContext

/**
 * A base class for a typical servlet using the XS framework. Takes care of the XS connections (images, comet, events) and separates out the user code.
 */
abstract class XSServlet extends HttpServlet {  
  
  import concurrent.ExecutionContext
  val executorService = java.util.concurrent.Executors.newFixedThreadPool(4)
  val executionContext = ExecutionContext.fromExecutorService(executorService)

  //val executionContext : ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  
  /** The main front page, for a get command without a "sub" parameter. This is what the user should override. */
  def mainPage(request:HttpServletRequest,response:HttpServletResponse)
  
  override def doGet(request:HttpServletRequest,response:HttpServletResponse) {
    request.getParameter("sub") match {
      case null => mainPage(request,response)
      case "icon" => icon(response,request.getParameter("name"),request.getDateHeader("If-Modified-Since"))
    }
  }


  IconManifests.urlOfIcon = new URLOfIcon {
    def apply(icon:ResolvedIcon) : String = "?sub=icon&name="+URLEncoder.encode(icon.fileName,"UTF-8")
  }
  def icon(response:HttpServletResponse,name:String,lastMod:Long) {
    if (name==null || name.isEmpty) response.sendError(HttpServletResponse.SC_BAD_REQUEST,"Query string doesn't make sense")
    else IconManifests.iconFromFilename(name) match {
      case None => response.sendError(HttpServletResponse.SC_NOT_FOUND)
      case Some(icon) if icon.contents.lastModified>0 &&  icon.contents.lastModified <= lastMod => response.sendError(HttpServletResponse.SC_NOT_MODIFIED) 
      case Some(icon) => 
        response.setContentType(icon.mimeType)
        if (icon.contents.lastModified>0) response.setDateHeader("Last-Modified",icon.contents.lastModified)
        val os = response.getOutputStream();
        os.write(icon.data)
        os.close()
    }
  }
  
  private val openCometThreadsSync = new Object
  private var openCometThreads : Set[Thread] = Set.empty
  
  /** Sending via comet or message */
  override def doPost(request:HttpServletRequest,response:HttpServletResponse) {
    val responseMessage = (for (sessionID<-Option(request.getParameter("xsSessionID"));e<-SessionManagement.get(sessionID)) yield e) match {
      case Some(session) =>
        request.getParameter("sub") match {
          case "comet" =>
            val t = Thread.currentThread()
            openCometThreadsSync.synchronized{ openCometThreads+=t }
            val res = try {
              Await.result(session.cometCallFuture,scala.concurrent.duration.Duration.Inf) // TODO should do asynchronously rather than await.
            } catch { case _:InterruptedException => None }
            openCometThreadsSync.synchronized{ openCometThreads-=t }
              //session.cometCallShouldReturnImmediately().orElse{session.cometCall()}
           // Thread.sleep(3000L) // simulate a slow connection
            res
          case "message" =>
            val message = ClientMessage.deserialize(request.getInputStream())
            println("Received "+message)
            //Thread.sleep((java.lang.Math.random()*3000).toInt) // simulate a slow connection with packet reordering
            //if (java.lang.Math.random()>0.8) throw new IllegalArgumentException // simulate a server error
            //if (java.lang.Math.random()>0.8) Thread.sleep(20000) // simulate a lost packet (with a long delay to stop error messages arriving)
            val messageCountString = request.getParameter("count")
            try {
              if (messageCountString!=null) Some(session.receivedPossiblyUnorderedMessage(message, messageCountString.toLong))
              else None
            } catch { case _:NumberFormatException => None }
          case _ => None
        }
      case None => Some(ClientMessage.lostSession)
    } 
    response.setContentType("text/json");
    responseMessage match {
      case Some(message) => message.serialize(response.getOutputStream())
      case None => response.getWriter().print("null")
    }
  }
  
  override def destroy() {
    println("Start destroy in XSServlet")
    var tokill = openCometThreads
    for (k<-tokill) k.interrupt()
    executorService.shutdown()
    SessionManagement.scheduler.shutdownNow()
    Thread.sleep(10)
    super.destroy()
    println("End destroy in XSServlet")
    //if (loadedProperly || !file.exists()) save()
  }
  

}  