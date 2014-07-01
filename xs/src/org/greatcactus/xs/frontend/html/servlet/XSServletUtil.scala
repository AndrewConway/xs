/**
 * Copyright 2014 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.frontend.html.servlet

import javax.servlet.http.HttpServletResponse
import org.greatcactus.xs.api.icon.IconManifests

/**
 * Some utilities when using XS from a servlet context.
 */
object XSServletUtil {
  
  /** Return an icon with the given name. Use the lastMod to see if hasn't changed */
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

}