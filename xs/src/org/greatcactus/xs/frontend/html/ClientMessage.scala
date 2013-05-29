/**
 * Copyright 2012-2013 Andrew Conway. All rights reserved
 */
package org.greatcactus.xs.frontend.html

import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.core.JsonGenerator
import java.io.OutputStream
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.JsonToken
import scala.collection.mutable.ArrayBuffer
import java.io.InputStream
import java.io.Reader
import scala.xml.NodeSeq
import scala.collection.mutable.ListBuffer
import org.greatcactus.xs.api.errors.ResolvedXSError
import org.greatcactus.xs.api.display.RichLabel
import java.io.StringWriter
import org.greatcactus.xs.frontend.ColumnExtractors

/**
 * Wrapper for commands sent to/from a HTML client, typically over HTTP or websockets.
 * Commands are encoded as JSON for ease of parsing on the client side.
 */

sealed abstract class ClientMessage {
  def serialize(g:JsonGenerator)
  def serialize(out:OutputStream) {
    val g = ClientMessage.jsonFactory.createJsonGenerator(out)
    serialize(g)
    g.close()
  }
}

case class SimpleClientMessage(val command:String,val args:Array[String]) extends ClientMessage {
  def serialize(g:JsonGenerator) {
    g.writeStartObject()
    g.writeStringField("cmd",command)
    if (args!=null && args.length>0) {
      g.writeArrayFieldStart("args")
      for (s<-args) g.writeString(s)
      g.writeEndArray()
    }
    g.writeEndObject()
  }
  override def toString = command+args.mkString("(" , "," , ")")
}

class ErrorClientMessage(val id:String,val errors:List[ResolvedXSError],val gridID:Option[String]) extends ClientMessage {
  def serialize(g:JsonGenerator) {
    g.writeStartObject()
    g.writeStringField("cmd","Errors")
    g.writeStringField("id",id)
    for (gid<-gridID) g.writeStringField("gridID",gid) 
    g.writeArrayFieldStart("errors")
    for (e<-errors){
      g.writeStartObject()
      g.writeNumberField("severity",e.severity.level);
      g.writeNumberField("from",e.from)
      g.writeNumberField("to",e.to)
      g.writeStringField("text",e.description.html.toString)
      g.writeArrayFieldStart("run")
      for (cmd<-e.description.postCreationJavascript) g.writeString(cmd)
      g.writeEndArray()
      g.writeEndObject()
    } 
    g.writeEndArray()
    g.writeEndObject()
  }
  override def toString = "Errors["+id+":"+errors.mkString(";")+"]"
  
}

class MultipleClientMessage(val commands:Seq[ClientMessage]) extends ClientMessage {
   def serialize(g:JsonGenerator) {
    g.writeStartObject()
    g.writeArrayFieldStart("commands")
    for (c<-commands) c.serialize(g)
    g.writeEndArray()
    g.writeEndObject()
  }
  override def toString = commands.mkString("[",";","]")
}

class SetRow(val baseid:String,val rownumber:Int,val row:IndexedSeq[String],val columnNames:Int=>String) extends ClientMessage {
   def serialize(g:JsonGenerator) {
    g.writeStartObject()
    g.writeStringField("cmd","SetRow")
    g.writeStringField("id",baseid)
    g.writeNumberField("num",rownumber)
    g.writeObjectFieldStart("row")
    for (i<-0 until row.length) g.writeStringField(columnNames(i),row(i))
    g.writeEndObject()
    g.writeEndObject()
  }
  override def toString = "SetRow("+baseid+","+rownumber+","+row.mkString(";")+")"  
}

class SetRows(val baseid:String,val rows:Seq[Seq[String]],val columnNames:Int=>String) extends ClientMessage {
   def serialize(g:JsonGenerator) {
    g.writeStartObject()
    g.writeStringField("cmd","SetRows")
    g.writeStringField("id",baseid)
    g.writeArrayFieldStart("rows")
    for (row<-rows) {
      g.writeStartObject()
      for (i<-0 until row.length) g.writeStringField(columnNames(i),row(i))
      g.writeEndObject()
    }
    g.writeEndArray()
    g.writeEndObject()
  }
  override def toString = "SetRows("+baseid+","+rows.map{_.mkString("[",";","]")}.mkString(";")+")"  
}

class SetGridRowMetadata(val baseid:String,val rows:Seq[Map[String,String]]) extends ClientMessage {
   def serialize(g:JsonGenerator) {
    g.writeStartObject()
    g.writeStringField("cmd","SetGridRowMetadata")
    g.writeStringField("id",baseid)
    g.writeArrayFieldStart("rows")
    for (row<-rows) {
      g.writeStartObject()
      for ((key,value)<-row) g.writeStringField(key, value)
      g.writeEndObject()
    }
    g.writeEndArray()
    g.writeEndObject()
  }
  override def toString = "SetGridRowMetadata("+baseid+","+rows.map{_.mkString("[",";","]")}.mkString(";")+")"  
  
}


class GridSetCellCssStyles(val baseid:String,illegalEntries:Map[Int,List[Int]],val columnNames:Int=>String,val cssStyle:String) extends ClientMessage {
   def serialize(g:JsonGenerator) {
    g.writeStartObject()
    g.writeStringField("cmd","GridSetCellCssStyles")
    g.writeStringField("id",baseid)
    g.writeStringField("key",cssStyle)
    g.writeObjectFieldStart("what")
    for ((row,cols)<-illegalEntries) {
      g.writeObjectFieldStart(""+row)
      for (col<-cols) try { g.writeStringField(columnNames(col),cssStyle) } catch { case _:Exception => } // catch takes care of case of where column names is not long enough.
      g.writeEndObject()
    }
    g.writeEndObject()
    g.writeEndObject()
  }
  override def toString = "SetRows("+baseid+","+illegalEntries.map{case (row,cols)=>cols.map{""+row+","+_}.mkString(";")}.mkString(";")+")"  
}


   
object ClientMessage {
  val jsonFactory = new JsonFactory()
  
  def deserialize(data:Array[Byte]) : ClientMessage = deserializeObjAndClose(jsonFactory.createJsonParser(data))
  def deserialize(data:String) : ClientMessage = deserializeObjAndClose(jsonFactory.createJsonParser(data))
  def deserialize(data:InputStream) : ClientMessage = deserializeObjAndClose(jsonFactory.createJsonParser(data))
  def deserialize(data:Reader) : ClientMessage = deserializeObjAndClose(jsonFactory.createJsonParser(data))

  private def deserializeObjAndClose(p:JsonParser) : ClientMessage = {
    val res = deserializeObj(p)
    p.close()
    res    
  }
  private def deserializeObj(p:JsonParser) : ClientMessage = {
    val st = p.nextToken() // should produce START_OBJECT
    assert(st==JsonToken.START_OBJECT)
    val f1t = p.nextToken() // should produce FIELD_NAME
    assert(f1t==JsonToken.FIELD_NAME)
    val fieldname = p.getCurrentName()
    p.nextToken()
    val res = if (fieldname=="commands") { // it is MultipleJavascriptCommands
        val a = new ArrayBuffer[ClientMessage]
        val sa = p.nextToken()
        assert (sa==JsonToken.START_ARRAY)
        while (p.nextToken()!=JsonToken.END_ARRAY) a+=deserializeObj(p)
        val eo = p.nextToken()
        assert(eo == JsonToken.END_OBJECT)
        new MultipleClientMessage(a.toSeq)
    } else if (fieldname=="cmd") { // it is SimpleJavascriptCommand
        val cmd = p.getText()
        val args = new ArrayBuffer[String]
        if (p.nextToken()!=JsonToken.END_OBJECT) {
          assert("args"==p.getCurrentName())
          val sa = p.nextToken()
          assert (sa==JsonToken.START_ARRAY)
          while (p.nextToken()!=JsonToken.END_ARRAY) args+=p.getText()
          val eo = p.nextToken()
          assert(eo == JsonToken.END_OBJECT)
        }
        new SimpleClientMessage(cmd,args.toArray)
    } else throw new IllegalArgumentException("Unknown field name "+fieldname)
    res
  }

  def setCurrentlyEditing(sessionID:String,nodeID:String) = new SimpleClientMessage("SetCurrentlyEditing",Array(sessionID,nodeID))
  def setAttributeID(id:String,att:String,value:String) = 
    if (value==null) new SimpleClientMessage("RemoveAtt",Array(id,att))
    else new SimpleClientMessage("SetAtt",Array(id,att,value))
  def removeID(id:String) = new SimpleClientMessage("Remove",Array("#"+id))
  def addAfterID(id:String,html:NodeSeq) = new SimpleClientMessage("AddAfter",Array("#"+id,html.toString))
  def addAtStartID(id:String,html:NodeSeq) = new SimpleClientMessage("AddAtStart",Array("#"+id,html.toString))
  def setVisibleID(id:String,visible:Boolean) = new SimpleClientMessage(if (visible) "Show" else "Hide",Array("#"+id))
  //def setEnabledID(id:String,enabled:Boolean) = new SimpleClientMessage(if (enabled) "Enable" else "Disable",Array("#"+id))
  def setValueID(id:String,value:String) = { new SimpleClientMessage("SetValue",Array(id,value)) }
  def setCheckedID(id:String,value:Boolean) = { new SimpleClientMessage("SetChecked",Array(id,value.toString)) }
  def changeErrors(id:String,errors:List[ResolvedXSError]) = new ErrorClientMessage(id,errors,None)
  def changeGridErrors(id:String,errors:List[ResolvedXSError],gridID:String) = new ErrorClientMessage(id,errors,Some(gridID))
  def gotoURL(url:String) = new SimpleClientMessage("GotoURL",Array(url))
  def gotoURLNewTab(url:String) = new SimpleClientMessage("GotoURLNewTab",Array(url))
  def errorMessage(message:String) = new SimpleClientMessage("message",Array(message))
  def setToolbarStatus(toolbarid:String,enabled:Boolean,html:String) = new SimpleClientMessage("ToolbarStatus",Array(toolbarid,enabled.toString,html))
  def setHTMLID(id:String,html:NodeSeq) = {
    new SimpleClientMessage("SetHTML",Array(id,html.toString))
  }
  def setHTMLID(id:String,html:String) = {
    new SimpleClientMessage("SetHTML",Array(id,html))
  }
  def setHTMLID(id:String,html:RichLabel) : ClientMessage = {
    if (html==null) setHTMLID(id,NodeSeq.Empty)
    else {
      val main = setHTMLID(id,html.html)
      html.postCreationJavascript match {
        case Nil => main
        case l => new MultipleClientMessage(main:: l.map{ClientMessage.run(_)})
      }
    }
  }
  def setHTMLIDnotBlank(id:String,html:RichLabel) : ClientMessage = {
    if (html==null) setHTMLID(id,"???")
    else {
      val main = setHTMLID(id,html.htmlNotBlank)
      html.postCreationJavascript match {
        case Nil => main
        case l => new MultipleClientMessage(main:: l.map{ClientMessage.run(_)})
      }
    }
  }
  
  def addClass(selector:String,className:String,add:Boolean) = new SimpleClientMessage(if (add) "AddClass" else "RemoveClass",Array(selector,className))
  def setFieldIllegalContentsID(id:String,isIllegal:Boolean) = addClass("#"+id,"xsTotallyIllegal",isIllegal)
  def lostSession = new SimpleClientMessage("LostSession",Array())
  def ackMessage(ackid:String) = new SimpleClientMessage("ACK",Array(ackid))
  def run(command:String) = new SimpleClientMessage("Run",Array(command))
  
  val defaultStartGridOptions = """[{"autoHeight":true, "editable":true, "enableAddRow":true, "enableCellNavigation":true, "fullWidthRows":true, "forceFitColumns":true}][0]""" // produces an error if I have l instead of [l][0]. I don't know why.
  def startGrid(baseid:String,columns:String,onInputObj:String,options:String=defaultStartGridOptions) = new SimpleClientMessage("StartGrid",Array(baseid,columns,onInputObj,options))
  def stopGrid(baseid:String) = new SimpleClientMessage("StopGrid",Array(baseid))
  def setGridRowCSSStyles(baseid:String,classes:Seq[String]) = {
    def asmap(c:String) : Map[String,String] = if (c==null || c.isEmpty) Map.empty else Map("cssClasses"->c) 
    new SetGridRowMetadata(baseid,classes.map{asmap}) // {c:String=>if (c==null || c.isEmpty) Map.empty else Map("cssClasses"->c)}) 
  }

  def progressStart(baseid:String) = new SimpleClientMessage("ProgressBar",Array(baseid,"Start"))
  def progressProgress(baseid:String,progress:Double) = new SimpleClientMessage("ProgressBar",Array(baseid,"Progress",progress.toString))
  def progressFinishedGood(baseid:String,message:Option[RichLabel]) = new SimpleClientMessage("ProgressBar",Array(baseid,"FinishedOK",message.getOrElse(RichLabel.nullLabel).html.toString))
  def progressFinishedError(baseid:String,message:Option[RichLabel]) = new SimpleClientMessage("ProgressBar",Array(baseid,"FinishedError",message.getOrElse(RichLabel.nullLabel).html.toString))
  
  def acknowledge(justReceived:Long,expectedNext:Long,biggestEverReceived:Long) = new SimpleClientMessage("ACK",Array(justReceived.toString,expectedNext.toString,biggestEverReceived.toString))
  
  def showCustomPopup(id:String,okJSFunction:String,resultJSFunction:String) = new SimpleClientMessage("ShowCustomPopup",Array(id,okJSFunction,resultJSFunction))
  def disposeCustomPopup(id:String) = new SimpleClientMessage("DisposeCustomPopup",Array(id))
  
  
  /** Paste data is converted into a 1D array, starting at index 4, with each row prepended by the length of the row */
  def unmungePasteGrid(args:Array[String]) : Array[Array[String]] = {
    val res = new ArrayBuffer[Array[String]]
    var upto = 4
    while (upto<args.length) {
      val len = args(upto).toInt
      upto+=1
      res+=args.slice(upto,upto+len)
      upto+=len
    }
    res.toArray
  }
}

/** If you want to store up client messages and merge into a bigger message, use this. */
class ClientMessageBuffer {
  private[this] val buffer = new ListBuffer[ClientMessage]
  
  def +=(message:ClientMessage) { synchronized { buffer+=message; }}
  
  def get() : Option[ClientMessage] = {
    synchronized {
      val cmds = buffer.toList
      buffer.clear()
      cmds match {
        case Nil => None
        case h::Nil => Some(h)
        case l => Some(new MultipleClientMessage(l))
      }  
    }
  }
  
}