/**
 * Copyright Andrew Conway 2013
 */
package org.greatcactus.xs.util

import java.io.InputStream
import scala.collection.mutable.ArrayBuffer

/**
 * Things that I wish were in std lib
 */
object FileUtil {

  def readAllOfInputStream(is:InputStream) : Array[Byte] = {
    val res = new ArrayBuffer[Array[Byte]]
    var finished = false
    while (!finished) {
      val buf = new Array[Byte](16384)
      val len = is.read(buf)
      if (len <= 0) finished=true
      else if (len==buf.length) res+=buf
      else res+=buf.slice(0,len)
    }
    is.close()
    if (res.length==1) res(0)
    else res.toArray.flatten
  }
}