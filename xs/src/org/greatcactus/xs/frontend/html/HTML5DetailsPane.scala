/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.frontend.html

import java.util.Locale
import scala.xml.Attribute
import scala.xml.Text
import scala.xml.NodeSeq
import scala.collection.mutable.ListBuffer
import org.greatcactus.xs.frontend._
import org.greatcactus.xs.api.icon.Icon
import org.greatcactus.xs.api.display.RichLabel
import org.greatcactus.xs.api.errors.ResolvedXSError
import scala.concurrent._
import ExecutionContext.Implicits.global
import org.greatcactus.xs.impl.GeneralizedField

/**
 * XSDetailsPane specialized for HTML5. This is not a complete implementation - it ignores transport.
 *
 * The type of a GUI object is a string representing the base of the identifier for the corresponding gui object.
 */
class HTML5DetailsPane(val client:HTML5Client) extends XSDetailsPane[String](client.locale,client.xsedit) {
  
  val detailsPaneID = "xsDP_"+client.session.jsSessionID // the ID of the main div holding all the details pane.
  
  var customControllerProcessMessages : List[PartialFunction[SimpleClientMessage,Unit]] = Nil
  
  /** Send a javascript command to set the inner html of the stated id to the given html. */
  def jsSetHTML(id:String,html:NodeSeq) {
    client.queueMessage(ClientMessage.setHTMLID(id,html))
  } 
  /** Send a javascript command to set the inner html of the stated id to the given html. */
  def jsSetHTML(id:String,html:RichLabel) {
    message( ClientMessage.setHTMLID(id,html))
  } 
  /** Send a javascript command to set the attribute to a given value (or remove the attribute if value is null) */
  def jsSetAttribute(id:String,att:String,value:String) {
    client.queueMessage(if (att=="value") ClientMessage.setValueID(id,value) else ClientMessage.setAttributeID(id,att,value))
  }
  def message(message:ClientMessage) { client.queueMessage(message) }
  
  override def dispose() {
    synchronized {
      super.dispose()
      customControllerProcessMessages=Nil
    }
  }

  def dispose(guis:UIFields) {
    // disposal is automatic, except for tables which need to be explicitly deallocated.
    for (uifield<-guis.elems) uifield match {
      case table:UIFieldTable => message(ClientMessage.stopGrid(table.gui))
      case custom:UIFieldCustom[_] => custom.work.dispose()
      case _ =>
    }
  } 

  override def getClipboard(param:XSClipboardRequest) : Future[XSClipBoard] = client.session.getClipboard(param)
  override def setClipboard(data:XSClipBoard) { client.session.setClipboard(data)}
    
  def setBlankScreen() { jsSetHTML(detailsPaneID,NodeSeq.Empty)}
  
  def flushClientCommands() { client.flushMessages() }
    
  def newCreator() = new GUICreatorHTML5(this)

  def changeUITextField(id:String,shouldBe:String) { jsSetAttribute(id+"_ui","value",shouldBe) }
  def changeUIImageField(id:String,shouldBe:String) { jsSetAttribute(id+"_image","src",shouldBe) }

  def changeUIBooleanField(id:String,shouldBe:Boolean) { client.queueMessage(ClientMessage.setCheckedID(id+"_ui",shouldBe)) }
  def changeUIShowText(id:String,shouldBe:RichLabel) { jsSetHTML(id+"_ui",shouldBe)}

  def changeUIvisibility(id:String,visible:Boolean) { client.queueMessage(ClientMessage.setVisibleID(id+"_all", visible)) } // hidden does not work on IE. jsSetAttribute(id+"_all","hidden",if (visible) null else "hidden") }
  def changeUIenabledness(id:String,enabled:Boolean)  { /* client.queueMessage(ClientMessage.setEnabledID(id+"_ui", enabled))} */ jsSetAttribute(id+"_ui","disabled",if (enabled) null else "disabled") }
  def changeUILabelText(id:String,shouldBe:RichLabel) { jsSetHTML(id+"_labeltext",shouldBe) }
  def changeUILabelIcon(id:String,shouldBe:Option[Icon]) { jsSetHTML(id+"_labelicon",GUICreatorHTML5.rawicon(shouldBe)) }
  def changeErrors(gui:String,errors:List[ResolvedXSError]) { client.queueMessage(ClientMessage.changeErrors(gui+"_ui",errors))}
  def changeGridErrors(gui:String,row:Int,col:Int,colfield:GeneralizedField,errors:List[ResolvedXSError]) { client.queueMessage(ClientMessage.changeGridErrors(gui+"_grid_R"+row+"C"+colfield.name, errors, gui))}
  def setUIFieldIllegalContents(gui:String,isIllegal:Boolean) { client.queueMessage(ClientMessage.setFieldIllegalContentsID(gui+"_ui",isIllegal))}
  def changeUIWholeTable(gui:String,shouldBe:IndexedSeq[IndexedSeq[String]]) { for (columns<-getColumnExtractor(gui)) message(new SetRows(gui,shouldBe,columns.names))} 
  def changeUISingleLineTable(gui:String,index:Int,shouldBe:IndexedSeq[String]) { for (columns<-getColumnExtractor(gui)) message(new SetRow(gui,index,shouldBe,columns.names))} 
  override def setUITableEntriesIllegalContents(gui:String,illegalEntries:Map[Int,List[Int]]) { for (columns<-getColumnExtractor(gui)) message(new GridSetCellCssStyles(gui,illegalEntries,columns.names,"xsTotallyIllegal"))} 
  /*override def changeUIShowCustom[S](gui:String,custom:CustomComponent[S,String],shouldBe:S,old:S) { 
    custom match {
      case drawer:HTMLCustomComponent[S] => 
        for (m<-drawer.change(gui,shouldBe,old)) message(m)
      case _ => 
    }
  }*/


  def baseHTML = <div class="XSDetailsPane" id={detailsPaneID}>Contacting server...</div>
  
  private def getColumnExtractor(uiElement:String) : Option[ColumnExtractors] = { 
    for (field<-uiField(uiElement) if field.isInstanceOf[UIFieldTable]) yield field.asInstanceOf[UIFieldTable].field.columnExtractors
  }
  
  
  override def setNodeIDCurrentlyBeingEdited(id:String) {
    super.setNodeIDCurrentlyBeingEdited(id)
    client.queueMessage(ClientMessage.setCurrentlyEditing(client.session.jsSessionID,id))
    nodeIDCurrentlyBeingEdited=id
  }

  def getCustom(f:DetailsPaneFieldCustom) : Option[CustomComponent[_,String]] = HTML5DetailsPane.getCustom(f)

}

object HTML5DetailsPane extends CustomComponentStore[String]

object GUICreatorHTML5 {
  
  
  
  def addOption(html:xml.Elem,name:String,value:Option[Any]) = value match {
    case Some(s) => html % Attribute(None,name,Text(s.toString),scala.xml.Null)
    case None => html
  }
  def addTitle(html:xml.Elem,tooltip:Option[String]) = addOption(html,"title",tooltip)
  private def addBoolean(html:xml.Elem,name:String,relevent:Boolean) = if (relevent) html % Attribute(None,name,Text(name),scala.xml.Null) else html
  private def addEnabled(html:xml.Elem,enabled:Boolean) = addBoolean(html,"disabled",!enabled)
  
  def iconlabel(ic:Option[Icon],currently:CurrentFieldState,id:String) : NodeSeq = {
    val html = rawicon(currently.specialIcon.orElse(ic))
    if (currently.canHaveSpecialIcon) <span id={id+"_labelicon"}>{html}</span> else html
  } 
  
  def rawicon(ic:Option[Icon]) = ic.flatMap{_.getInstance(Icon.html5formats,Some(32))} match {
    case Some(image) => <img class="xsIcon" src={image.url}/>
    case None => NodeSeq.Empty
  }
  

}

/** 
 * A GUI creator for HTML5, where the definer of an object is its id field - a string. 
 * It is laid out in a table. We want the second column to get extra space - the first is set to a small width. <td style="width: 1px;">
 * */
class GUICreatorHTML5(pane:HTML5DetailsPane) extends GUICreator[String] {
  import GUICreatorHTML5._
  private[this] var idIndex = 0 
  private[this] val idBase = "XS_Form"+pane.client.session.jsSessionID+"_" 
  private[this] def newid() = { idIndex+=1; idBase+idIndex }
  private[this] val rowBuffer = new ListBuffer[xml.Elem]
  private[this] val sectionBuffer = new ListBuffer[xml.Elem]
  val postCreationJavascript = new ListBuffer[ClientMessage]
  
  private def addRow(row:xml.Elem,visible:Boolean) { 
    val withvisibility = XSHTMLUtil.possiblySetNoDisplay(row,visible)
    //println("Adding row "+row)
    rowBuffer+=withvisibility 
  }
  private def extractRows() : NodeSeq = {
    val res : NodeSeq = rowBuffer.toList
    rowBuffer.clear()
    <table class="xsForm">{res}</table>
  }
  
  /** Add post processing instructions from a rich label onto the queue for this pane */
  private def dealWithPostProcessing(l:RichLabel) {
     for (cmd<-l.postCreationJavascript) postCreationJavascript+=ClientMessage.run(cmd)     
  }
  def textlabel(s:String,currently:CurrentFieldState,id:String) : NodeSeq = {
    val html = currently.specialLabel match {
      case Some(l) => 
        dealWithPostProcessing(l)
        l.html
      case None => Text(s)
    }
    if (currently.canHaveSpecialLabel) <span id={id+"_labeltext"}>{html}</span> else html
  } 

  def startSection(section:DetailsPaneFieldSection,currently:CurrentFieldState) = newid()
  
  def wrap(s:Option[String],f:String=>NodeSeq) = s match { case Some(t) => f(t); case None => NodeSeq.Empty}
  def endSection(section:DetailsPaneFieldSection,id:String,currently:CurrentFieldState) {
    val html = 
      <fieldset id={id+"_all"} class="xsErrorLabeled">
        { errorIcon(id,section.couldContainErrorIcon)}
        { (for (t<-section.title) yield <legend>{ iconlabel(section.icon,currently,id)}{textlabel(t,currently,id)}</legend>).getOrElse(NodeSeq.Empty) }
        { (for (t<-section.description) yield <p>{t}</p>).getOrElse(NodeSeq.Empty) }
        { extractRows() }
      </fieldset>
    val withenabled = addEnabled(html,currently.enabled)      
    val withvisibility = XSHTMLUtil.possiblySetNoDisplay(withenabled,currently.visible)
    sectionBuffer+=withvisibility
  }
  
  override def startForm(title:DetailsPaneFieldSection,currently:CurrentFieldState) : String = {
    val id = newid();
    if (title.couldContainErrorIcon) sectionBuffer+=errorIcon(id)
    title.title match {
      case Some(titleString) => 
        sectionBuffer+=addTitle(<h3 id={id+"_labeltext"}>{iconlabel(title.icon,currently,id)}{textlabel(titleString,currently,id)}</h3>,title.tooltip)
      case _ =>
    }
    title.description match {
      case Some(info) => sectionBuffer+= <p>{info}</p>
      case None =>
    }
    id;
  }
  
  def sessionprefix : String = pane.client.session.sessionPrefix
  def sessionPrefixNoTrailingPeriod = pane.client.session.sessionPrefixNoTrailingPeriod
  
  def endForm() {
    val res : NodeSeq = sectionBuffer.toList
    //println(res)
    sectionBuffer.clear()
    pane.jsSetHTML(pane.detailsPaneID,res)
    for (m<-postCreationJavascript) pane.message(m)
  }
  def createAction(field:DetailsPaneFieldAction,currently:CurrentFieldState) : String = {
    val id = newid()
    val link = addTitle(<a id={id+"_ui"} href="javascript:void(0)" onclick={sessionprefix+"action('"+id+"');false"}>{iconlabel(field.icon,currently,id)}{textlabel(field.label,currently,id)}</a>,field.tooltip)
    val withenabled = addEnabled(link,currently.enabled)
    val inrow = <tr id={id+"_all"}><td colspan="2">{withenabled}</td></tr>
    addRow(inrow,currently.visible)
    id
  }
  def createTextField(field:DetailsPaneFieldText,currently:CurrentFieldState,initialValue:String) : String = {
    val id = newid()
    val input = field.choices match {
      case Some(choicespec) if choicespec.required =>
        <select id={id+"_ui"} onchange={sessionprefix+"change('"+id+"')"}>{
          if (field.required) NodeSeq.Empty else <option value=""></option>
          }{
          for (c<-choicespec.choices) yield { // it would be nice to include {icon(c.icon)} in the <option></option> field, but it is not allowed in HTML. Could make a fancy select replacement using javascript... eg jqueryui of bootstrap.
            val option = <option value={c.original}>{c.localized}</option>
            if (c.original==initialValue) option% Attribute(None,"selected",Text("selected"),scala.xml.Null) else option
          }
        }</select>
      case _ => // TODO use info like placeholder, etc. */
        val ptf = <div id={id+"_ui"} contenteditable="true" class="xsPseudoTextField" spellcheck="false" oninput="xsPTF.input(event)" onkeyup="xsPTF.inputSurrogate(event)" onblur="xsPTF.inputSurrogate(event)" onpaste="xsPTF.inputSurrogate(event)" oncut="xsPTF.inputSurrogate(event)" data-onInputObj={sessionPrefixNoTrailingPeriod} data-xsSuppressNL={(!field.multiline).toString}></div>
        postCreationJavascript+=ClientMessage.setValueID(id+"_ui",initialValue)
        ptf
        /*
        val rawinput = 
          if (field.multiline) <textarea id={id+"_ui"}>{initialValue}</textarea>
          else <input id={id+"_ui"} value={initialValue} type={field.knownType.getOrElse("text")} onchange={sessionprefix+"change('"+id+"')"} oninput={sessionprefix+"keyPress('"+id+"')"}/>
        val withoptions = addOption(addOption(addOption(rawinput,"maxlength",field.maxlength),"pattern",field.regex),"placeholder",field.placeholder)
        val fullinput = addEnabled(addBoolean(addBoolean(withoptions,"required",field.required),"readonly",field.readonly),currently.enabled)
        fullinput*/
    }
    addLabeledField(input,id,field,currently)
    id
  }
  def createImageField(field:DetailsPaneFieldImage,currently:CurrentFieldState,initialValue:String) = {
    val id = newid()
    val imagePortion = <img id={id+"_image"} class="xsImageField" src={if (initialValue==null) "" else initialValue} alt="📷 "/>
    val input = <div id={id+"_ui"} class="xsImageField" ondrop={sessionprefix+"imageDrop('"+id+"',event);"} ondragover={sessionprefix+"imageDrag('"+id+"',event);"} ondragenter={sessionprefix+"imageDrag('"+id+"',event);"}>{imagePortion}</div>
    val inputWithResolve = if (field.resolveNetworkReferences) addOption(input,"data-ResolveNetworkReferences",Some("true")) else input
    addLabeledField(input,id,field,currently)
    id    
  }
  def createTableField(field:DetailsPaneFieldTable,currently:CurrentFieldState,initialValue:IndexedSeq[IndexedSeq[String]]) = {
    val id = newid()
    val holder = <div id={id+"_ui"} class="xsTableHolder"><div id={id+"_grid"} class="xsTableGrid"></div><div class="xsTableTooltip"></div></div>
    //val baseVarID = "document.getElementById('"+id+"_ui').dataxs"
    //val varid = baseVarID+"_sg"
    val useDummyHeader = false; // put a set of row headers on the table, useful (but not necessary) for drag and drop. If used, need to alter the selection function to allow selection of non-focussable elements 
    val useColumns = if (useDummyHeader) null::field.columns else field.columns
    val columns = JSUtil.makeArray(useColumns)((c,g)=>{
      if (c==null) { // dummy head column for selection
        g.writeStringField("name","")
        g.writeStringField("field","#")
        g.writeNumberField("width",20)
        g.writeStringField("behavior","selectAndMove")
        g.writeBooleanField("selectable", false)
        g.writeBooleanField("focusable", false)
        g.writeBooleanField("resizable", false)
        g.writeStringField("cssClass","xsCellReorder dnd")
      } else {
        val (formatter,editor) = c match {
          case t:DetailsPaneFieldText => 
            g.writeBooleanField("multiline",t.multiline)
            t.choices match {
              case Some(choicespec) if choicespec.required =>
                g.writeArrayFieldStart("choices")
                if (!t.required) {
                  g.writeStartObject()
                  g.writeStringField("original","")
                  g.writeStringField("localized","")
                  g.writeEndObject()                  
                } 
                for (c<-choicespec.choices) { // it would be nice to include {icon(c.icon)} in the <option></option> field, but it is not allowed in HTML. Could make a fancy select replacement using javascript... eg jqueryui of bootstrap.
                  g.writeStartObject()
                  g.writeStringField("original",c.original)
                  g.writeStringField("localized",c.localized)
                  g.writeEndObject()
                }
                g.writeEndArray()
                ("xs.grid.SlickGridChoiceFormatter","xs.grid.SlickGridChoiceEditor")
              case _ => ("xs.grid.SlickGridPTFFormatter","xs.grid.SlickGridPTFEditor")
            }
          case b:DetailsPaneFieldBoolean =>
            ("xs.grid.SlickGridBooleanFormatter","xs.grid.SlickGridBooleanEditor")
          case _ => throw new IllegalArgumentException("Invalid grid field "+c)
        }
        g.writeStringField("name",c.label)
        g.writeStringField("field",c.name)
        g.writeStringField("id",c.name)
        g.writeStringField("mainID",id)
        g.writeBooleanField("resizable", true)
        //g.writeStringField("cssClass","dnd")
        g.writeStringField("behavior","selectAndMove")
        g.writeFieldName("formatter")
        g.writeRawValue(formatter)
        g.writeFieldName("editor")
        g.writeRawValue(editor)       
      }
    })
    postCreationJavascript+=new SetRows(id,initialValue,field.columnExtractors.names);
    postCreationJavascript+=ClientMessage.startGrid(id,columns,sessionPrefixNoTrailingPeriod)
    //run(varid+"=new Slick.Grid('#"+id+"_ui',"+baseVarID+"_rows,"+columns+",{autoHeight:true,editable:true,enableAddRow:true,enableCellNavigation:true,fullWidthRows:true})")
    //postCreationJavascript+=ClientMessage.run(varid+".setSelectionModel(new Slick.CellSelectionModel())")
    //postCreationJavascript+=ClientMessage.run(varid+".render()")
    addLabeledField(holder,id,field,currently)
    id    
  }

  override def createCustom[S](field:DetailsPaneFieldCustom,custom:CustomComponent[S,String],currently:CurrentFieldState,initialValue:S) : String = {
    val id = newid()
    val contents = custom match {
      case drawer:HTMLCustomComponent[S] =>
        drawer.createHTML(id,field,initialValue,this)
      case _ => <em>Missing custom component {custom.name}</em>
    }
    val holder = <div id={id+"_ui"} class={"xsCustom"+custom.name}>{contents}</div>
    addLabeledField(holder,id,field,currently)
    id
  }
  def errorIcon(id:String) : xml.Elem = <div class="xsErrorIconHolder"><div class="xsErrorIcon xsErrorIcon1000" id={id+"_ui_erroricon"}><div id={id+"_ui_errortooltip"}></div></div></div>
  def errorIcon(id:String,canContainErrorIcon:Boolean) : NodeSeq = if (canContainErrorIcon) errorIcon(id) else NodeSeq.Empty

  def addLabeledField(input:xml.Elem,id:String,field:DetailsPaneFieldLabeled,currently:CurrentFieldState) {
    val withenabled = addEnabled(input,currently.enabled)      
    val inrow = {
      def mainTD(colspan:Int) : xml.Elem = <td colspan={colspan.toString} class={"xsErrorLabeled xsColMainWidth"+colspan}>{errorIcon(id,field.couldContainErrorIcon)}{withenabled}</td>
      if (field.hideName) <tr id={id+"_all"}>{mainTD(2)}</tr>
      else {
        val label = addTitle(<label for={id+"_ui"}>{iconlabel(field.icon,currently,id)}{textlabel(field.label,currently,id)}</label>,field.tooltip)
        if (field.wholeLine) <tbody id={id+"_all"}><tr><td colspan="2" >{label}</td></tr><tr>{mainTD(2)}</tr></tbody>
        else <tr id={id+"_all"}><td class="xsColLabel">{label}</td>{mainTD(1)}</tr>
      }
    }
    addRow(inrow,currently.visible) 
  }
  
  def createBooleanField(field:DetailsPaneFieldBoolean,currently:CurrentFieldState,initialValue:Boolean) : String = {
    val id = newid()
    val inputUnchecked = <input type="checkbox" id={id+"_ui"} onchange={sessionprefix+"changeCB('"+id+"')"}/>
    val input = if (initialValue) inputUnchecked % Attribute(None,"checked",Text("checked"),scala.xml.Null) else inputUnchecked
    addLabeledField(input,id,field,currently)
    id     
  }
    /** Called to create a just-display text field (inside a section) */
  def createShowTextField(field:DetailsPaneFieldShowText,currently:CurrentFieldState,initialValue:RichLabel) : String = {
    val id = newid()
    dealWithPostProcessing(initialValue)
    val input = <div id={id+"_ui"}>{initialValue.html}</div>
    addLabeledField(input,id,field,currently)
    id 
  }

}


