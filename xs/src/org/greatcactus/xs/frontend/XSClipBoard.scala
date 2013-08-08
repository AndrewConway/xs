/**
 * Copyright (c) 2013 Silicon Econometrics Pty. Ltd. All rights reserved.
 */
package org.greatcactus.xs.frontend

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import java.io.DataInputStream
import java.io.ByteArrayInputStream

/**
 * Information that is stored in a clipboard. Contents may change.
 */
class XSClipBoard(val data:Array[Byte],val datatype:XSClipboardRequest) {
  def tabular: IndexedSeq[IndexedSeq[String]] = {
    val in = new DataInputStream(new ByteArrayInputStream(data))
    val len = in.readInt()
    println("Expecting "+len+" rows")
    for (i<-0 until len) yield {
      val rowlen = in.readInt()
      println("Expecting "+rowlen+" length row")
      for (j<-0 until rowlen) yield in.readUTF()
    }
  }
  
  override def toString = "Clipboard type "+datatype+" data "+new String(data)
}


object XSClipBoard {
  def serialized(data:Array[Byte]) = new XSClipBoard(data,XSClipboardRequest.xsSerializedData)
  def tabular(rows:IndexedSeq[IndexedSeq[String]]) : XSClipBoard = {
    val serializer = new ByteArrayOutputStream()
    val data = new DataOutputStream(serializer)
    data.writeInt(rows.length)
    for (row<-rows) {
      data.writeInt(row.length)
      for (s<-row) data.writeUTF(if (s==null) "" else s) // TODO this is limited to 64k strings - redo somehow
    }
    data.flush()
    new XSClipBoard(serializer.toByteArray(),XSClipboardRequest.tabularData)
  }
}
class XSClipboardRequest {
  
}

object XSClipboardRequest {
  val tabularData = new XSClipboardRequest
  val xsSerializedData = new XSClipboardRequest
}