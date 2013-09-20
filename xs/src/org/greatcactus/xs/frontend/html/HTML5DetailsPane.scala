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
import scala.concurrent.Future
import org.greatcactus.xs.impl.GeneralizedField
import org.greatcactus.xs.api.command.ProgressMonitor
import org.greatcactus.xs.api.command.ProgressMonitorUI
import scala.xml.transform.RewriteRule
import scala.xml.Elem
import scala.xml.Node
import scala.xml.transform.RuleTransformer
import org.greatcactus.xs.api.command.EditCommandDescription
import org.greatcactus.xs.api.command.EditCommandDescriptionMadeConcrete
import java.net.URLEncoder

/**
 * XSDetailsPane specialized for HTML5. This is not a complete implementation - it ignores transport.
 *
 * The type of a GUI object is a string representing the base of the identifier for the corresponding gui object.
 */
class HTML5DetailsPane(val client:HTML5Client) extends XSDetailsPane[String](client.locale,client.xsedit,client.executionContext) {
  
  val detailsPaneID = "xsDP_"+client.session.jsSessionID // the ID of the main div holding all the details pane.
  
  
  private object CustomControllerProcessMessageBuffer {
    private var fullList : List[PartialFunction[SimpleClientMessage,Unit]] = Nil
    private var fullListValid = false
    def list : List[PartialFunction[SimpleClientMessage,Unit]] = synchronized {
      if (!fullListValid) {
        fullList = bySource.values.toList.flatten
        fullListValid=true
      }
      fullList
    }
    // the first argument is the gui field of the UIFieldCustom[_] object.
    private var bySource : Map[String,List[PartialFunction[SimpleClientMessage,Unit]]] = Map.empty
    // the first argument is the gui field of the UIFieldCustom[_] object.
    def add(gui:String,p:PartialFunction[SimpleClientMessage,Unit]) : Unit = synchronized {
      val oldlist = bySource.get(gui).getOrElse(Nil)
      bySource=bySource+(gui -> (p::oldlist))
      fullListValid=false
    }
    def dispose() { synchronized {
      fullListValid=false;
      fullList=Nil
      bySource=Map.empty
    }}
    def dispose(gui:String) { synchronized {
      fullListValid=false;
      fullList=Nil
      bySource-=gui
    }}
  }
  
  def addCustomControllerProcessMessages(gui:String,p:PartialFunction[SimpleClientMessage,Unit]) {
    CustomControllerProcessMessageBuffer.add(gui,p)
  }
  
  def customControllerProcessMessages : List[PartialFunction[SimpleClientMessage,Unit]] = {
    var res = CustomControllerProcessMessageBuffer.list
    for (popup<-currentlyShowingPopup;m<-popup.processMessages) res=m::res
    return res
  }
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
    val msg = if (att=="value") ClientMessage.setValueID(id,value) else ClientMessage.setAttributeID(id,att,value)
    //if (att=="value") println("jsSetAttribute "+id+" = "+value)
    message(msg)
  }
  def message(message:ClientMessage) { client.queueMessage(message) }
  
  override def dispose() {
    synchronized {
      super.dispose()
      CustomControllerProcessMessageBuffer.dispose()
    }
  }

  def dispose(guis:UIFields) {
    // disposal is automatic, except for tables which need to be explicitly deallocated.
    for (uifield<-guis.elems) uifield match {
      case table:UIFieldTable => message(ClientMessage.stopGrid(table.gui))
      case custom:UIFieldCustom[_] => 
        custom.work.dispose()
        CustomControllerProcessMessageBuffer.dispose(custom.gui)
      case inline:UIFieldInline =>
        inline.dispose()
      case _ =>
    }
    synchronized {
      currentlyShowingPopup match {
        case Some(p) => 
          for (f<-p.closedByServer) f()
          message(ClientMessage.disposeCustomPopup(p.id))
        case _ =>
      }
      currentlyShowingPopup=None
      // fullCustomControllerProcessMessages=Nil // may not be nil in the presence of inline elements.
    }
  } 
  override def remove(gui:String) : Unit = { message(ClientMessage.removeID(gui))}
  override def getClipboard(param:XSClipboardRequest) : Future[XSClipBoard] = client.session.getClipboard(param,client.executionContext)
  override def setClipboard(data:XSClipBoard) { client.session.setClipboard(data)}
    
  def setBlankScreen() { jsSetHTML(detailsPaneID,NodeSeq.Empty)}
  
  override def flushClientCommands() { client.flushMessages() }
    
  def newCreator() = new GUICreatorHTML5(this,None)

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
  
  override def changeGridTooltip(gui:String,row:Int,col:Int,colfield:GeneralizedField,tooltip:Option[RichLabel]) { message(ClientMessage.setGridTooltip(gui+"_grid_R"+row+"C"+colfield.name,tooltip.getOrElse(RichLabel.nullLabel).html,gui+"_ui"))} // FIXME also should put them there in the first place maybe
  override def changeUITooltip(id:String,tooltip:Option[RichLabel]) {  jsSetHTML(id+"_tooltip",tooltip.getOrElse(RichLabel.nullLabel)) } 
  override def changeUIShowCommands(id:String,shouldBe:List[EditCommandDescriptionMadeConcrete]) {jsSetHTML(id+"_ui",GUICreatorHTML5.editCommandsHTML(id,client.session.sessionPrefix,shouldBe))}

  /*override def changeUIShowCustom[S](gui:String,custom:CustomComponent[S,String],shouldBe:S,old:S) { 
    custom match {
      case drawer:HTMLCustomComponent[S] => 
        for (m<-drawer.change(gui,shouldBe,old)) message(m)
      case _ => 
    }
  }*/

  private var currentlyShowingPopup : Option[HTMLCustomPopupComponentInfo] = None
  
  def popupMungeOutput(s:String) = currentlyShowingPopup match {
    case Some(popup) => popup.serverSideProcessResultFunction match {
      case Some(f) => 
        //println("In "+s+" out "+f(s))
        f(s)
      case None => s
    }
    case None => s
  }
  
  override def initiatePopup(field:UIFieldText,popupName:String,node:XSTreeNode) {
    if (currentlyShowingPopup.isDefined) {
      println("Already showing popup")
    } else HTML5DetailsPane.getPopup(popupName) match {
      case Some(p:HTMLCustomPopupComponent) =>
        val id = field.gui
        val info = p.createHTML(id,field.field,field.field.get(node),this,node.blockingGetDependencyInjectionInformation _)
        currentlyShowingPopup=Some(info)
        message(ClientMessage.setHTMLID(id+"_popup",info.html))
        message(ClientMessage.setAttributeID(id+"_popup","title",field.field.label))
        for (m<-info.postCreationJS) message(m)
        message(ClientMessage.showCustomPopup(field.gui,info.okJSFunction,info.resultJSFunction))
        flushClientCommands()
      case Some(_) => println("Bad type of popup "+popupName+" for field "+field.field)
      case None => println("No such popup "+popupName+" for field "+field.field)
    }
  }
  
  
  def uiClosedPopup(id:String) {
    synchronized {
      currentlyShowingPopup match {
        case Some(p) => 
          for (f<-p.closedByClient) f()
          currentlyShowingPopup=None
        case _ =>
      }
    }  
  }

  def baseHTML = <div class="XSDetailsPane" id={detailsPaneID}>Contacting server...</div>
  
  private def getColumnExtractor(uiElement:String) : Option[ColumnExtractors] = { 
    for (field<-uiField(uiElement) if field.field.isInstanceOf[UIFieldTable]) yield field.field.asInstanceOf[UIFieldTable].field.columnExtractors
  }
  
  
  override def setNodeIDCurrentlyBeingEdited(id:String) {
    super.setNodeIDCurrentlyBeingEdited(id)
    client.queueMessage(ClientMessage.setCurrentlyEditing(client.session.jsSessionID,id))
    nodeIDCurrentlyBeingEdited=id
  }

  def getCustom(f:DetailsPaneFieldCustom) : Option[CustomComponent[_,String]] = HTML5DetailsPane.getCustom(f)

  override def storeSelectedOnClient(s:String) { 
    message(ClientMessage.changeURLQuery(if (s==null || s.length>500) "" else "selected="+URLEncoder.encode(s,"UTF-8")))
  }

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
  
  def editCommandsHTML(id:String,sessionprefix:String,commands:List[EditCommandDescriptionMadeConcrete]) : NodeSeq = {
    var res:NodeSeq = NodeSeq.Empty
    for ((c,i) <-commands.zipWithIndex) yield {
      val subid = id+"_"+i
      val baseLink = <a id={subid+"_ui"} href="javascript:void(0)" onclick={sessionprefix+"action('"+id+"',"+i+");false"}>{rawicon(c.icon)}{c.label.html}</a>
      val withTitle = c.tooltip match {
        case None => baseLink
        case Some(text) => <div class="xsTooltipHolder"><div class="xsTooltip">{text.html}</div>{baseLink}</div>
      }
      // addEnabled(withTitle,enabled)
      withTitle
    }
  }

}

/** 
 * A GUI creator for HTML5, where the definer of an object is its id field - a string. 
 * It is laid out in a table. We want the second column to get extra space - the first is set to a small width. <td style="width: 1px;">
 * */
class GUICreatorHTML5(pane:HTML5DetailsPane,inlineParentDivId:Option[String]) extends GUICreator[String] {
  import GUICreatorHTML5._
  private[this] var idIndex = 0 
  private[this] val idBase = inlineParentDivId.map{_+"inline_"}.getOrElse("XS_Form"+pane.client.session.jsSessionID+"_") 
  private[this] def newid() = { idIndex+=1; idBase+idIndex }
  private[this] val rowBuffer = new ListBuffer[xml.Elem]
  private[this] val sectionBuffer = new ListBuffer[xml.Elem]
  private[this] var template:Option[FillInTemplate] = None
  val postCreationJavascript = new ListBuffer[ClientMessage]
  
  override def getExecutionContext = pane.client.executionContext
  private def addRow(row:xml.Elem,visible:Boolean,fieldName:String,field:NodeSeq,label:NodeSeq) { 
    val withvisibility = XSHTMLUtil.possiblySetNoDisplay(row,visible)
    for (t<-template) t.add(fieldName,field,label,withvisibility)
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
  
  override def startForm(title:DetailsPaneFieldSection,currently:CurrentFieldState,template:Option[Node]) : String = {
    this.template=template.map{new FillInTemplate(_)}
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
  
  override def endForm() = {
    val id = newid()
    val contents : NodeSeq = template match {
      case Some(t) => t.get
      case None => sectionBuffer.toList
    } 
    template=None
    val res = <div id={id} class="xsFormWrapper">{contents}</div>
    //println(res)
    sectionBuffer.clear()
    inlineParentDivId match {
      case Some(divid) => pane.message(ClientMessage.addAtEndID(divid,res))
      case None => pane.jsSetHTML(pane.detailsPaneID,res) 
    }
    for (m<-postCreationJavascript) pane.message(m)
    postCreationJavascript.clear()
    id
  }
  override def createInlineField(field:DetailsPaneFieldInline,currently:CurrentFieldState) : (String,GUICreator[String]) = {
    val id = newid()
    val divid = id+"_ui"
    val extraclass = if (field.noBorder) "" else " xsInlineFieldBorder"
    val input = <div class={"xsInlineField"+extraclass} id={divid}></div>
    addLabeledField(input,id,field,currently)
    (id,new GUICreatorHTML5(pane,Some(divid)))
  }

  def createAction(field:DetailsPaneFieldAction,currently:CurrentFieldState) : String = {
    val id = newid()
    val link = addTitle(<a id={id+"_ui"} href="javascript:void(0)" onclick={sessionprefix+"action('"+id+"');false"}>{iconlabel(field.icon,currently,id)}{textlabel(field.label,currently,id)}</a>,field.tooltip)
    val withenabled = addEnabled(link,currently.enabled)
    val inrow = <tr id={id+"_all"}><td colspan="2">{withenabled}</td></tr>
    addRow(inrow,currently.visible,field.name,withenabled,NodeSeq.Empty)
    id
  }
  override def createProgressMonitor(id:String) : ()=>ProgressMonitor = () => {
    pane.message(ClientMessage.progressStart(id))
    pane.flushClientCommands()
    val ui = new ProgressMonitorUI {
      def donePortion(progressPortion:Double) { pane.message(ClientMessage.progressProgress(id, progressPortion)); pane.flushClientCommands()}  
      def failed(message:Option[RichLabel]) { pane.message(ClientMessage.progressFinishedError(id, message)); pane.flushClientCommands() }
      def succeeded(message:Option[RichLabel]) { pane.message(ClientMessage.progressFinishedGood(id, message)); pane.flushClientCommands() }
    }
    ProgressMonitor(ui)
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
        //println("Post creation set "+id+" to "+initialValue)
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
    val holder = <div id={id+"_ui"} class="xsTableHolder"><div id={id+"_grid"} class="xsTableGrid"></div><div class="xsTableTooltip xsTableTooltipNormal"></div><div class="xsTableTooltip xsTableTooltipError"></div></div>
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
                ("xs.grid.SlickGridChoiceFormatter",if (t.readonly) null else "xs.grid.SlickGridChoiceEditor")
              case _ => ("xs.grid.SlickGridPTFFormatter",if (t.readonly) null else "xs.grid.SlickGridPTFEditor")
            }
          case b:DetailsPaneFieldBoolean =>
            ("xs.grid.SlickGridBooleanFormatter",if (b.readonly) null else "xs.grid.SlickGridBooleanEditor")
          case _ => throw new IllegalArgumentException("Invalid grid field "+c)
        }
        g.writeStringField("name",c.label)
        g.writeStringField("field",c.name)
        for (t<-c.tooltip) g.writeStringField("toolTip",t)
        g.writeStringField("id",c.name)
        g.writeStringField("mainID",id)
        g.writeBooleanField("resizable", true)
        //g.writeStringField("cssClass","dnd")
        g.writeStringField("behavior","selectAndMove")
        g.writeFieldName("formatter")
        g.writeRawValue(formatter)
        g.writeFieldName("editor")
        if (editor==null) g.writeNull() else g.writeRawValue(editor)       
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
    val customPopup : Option[HTMLCustomPopupComponent] = field match {
      case t:DetailsPaneFieldText =>
        //println(t.field.customPopup)
        for (popupName<-t.field.customPopup;p<-HTML5DetailsPane.getPopup(popupName)) yield p.asInstanceOf[HTMLCustomPopupComponent]
      case _ => None
    }
    //println(customPopup)
    val customPopupHTML = customPopup match {
      case None => NodeSeq.Empty
      case Some(popup) => <button class="xsPopupButton" onclick={sessionprefix+"initiatePopup('"+id+"')"}>…</button><div id={id+"_popup"}></div> 
    }
    val withenabled = addEnabled(input,currently.enabled) 
    val withTooltip = if (currently.canHaveTooltip) {
         <div class="xsTooltipHolder"><div class="xsTooltip" id={id+"_tooltip"}>{currently.tooltip match { case Some(t) => t.html; case None => NodeSeq.Empty}}</div>{withenabled}</div>
      } else withenabled
    val fieldHTML : NodeSeq = errorIcon(id,field.couldContainErrorIcon)++customPopupHTML++withTooltip
    val label = addTitle(<label for={id+"_ui"}>{iconlabel(field.icon,currently,id)}{textlabel(field.label,currently,id)}</label>,field.tooltip)
    val inrow = {
      def mainTD(colspan:Int) : xml.Elem = <td colspan={colspan.toString} class={"xsErrorLabeled xsColMainWidth"+colspan}>{fieldHTML}</td>
      if (field.hideName) <tr id={id+"_all"}>{mainTD(2)}</tr>
      else {
        if (field.wholeLine) <tbody id={id+"_all"}><tr><td colspan="2" >{label}</td></tr><tr>{mainTD(2)}</tr></tbody>
        else <tr id={id+"_all"}><td class="xsColLabel">{label}</td>{mainTD(1)}</tr>
      }
    }
    addRow(inrow,currently.visible,field.name,fieldHTML,label) 
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

  def createEditCommands(field:DetailsPaneFieldEditCommands,currently:CurrentFieldState,initialValue:List[EditCommandDescriptionMadeConcrete]) : String = {
    val id = newid()
    // dealWithPostProcessing(initialValue)
    val input = <div id={id+"_ui"}>{editCommandsHTML(id,sessionprefix,initialValue)}</div>
    addLabeledField(input,id,field,currently)
    id     
  }


}

class FillInTemplate(val template:Node) {
  var fields : Map[String,NodeSeq] = Map.empty
  var labels : Map[String,NodeSeq] = Map.empty
  var trs : Map[String,NodeSeq] = Map.empty

  def add(name:String,field:NodeSeq,label:NodeSeq,tr:NodeSeq) {
    fields+=name->field
    labels+=name->label
    trs+=name->tr
  }
  
  def get : NodeSeq = {
    //println("template = "+template)
    object t1 extends RewriteRule {
      override def transform(n: Node): Seq[Node] = { 
        //println("Matching node "+n)
        n match {
          case Elem("field",fieldName, attribs, scope, children @ _*) => fields.get(fieldName).getOrElse(NodeSeq.Empty)
          case Elem("fieldib",fieldName, attribs, scope, children @ _*) => fields.get(fieldName) match {
            case Some(field) =>
              val width = attribs("width") match { case null => "" ; case w => "width:"+(w.text)+";" }
              val style = "display:inline-block;" + width
              <div style={style} class="xsErrorLabeled">{field}</div>
            case None => NodeSeq.Empty
          }
          case Elem("xs","pack", attribs, scope, children @ _*) =>
            val revkids = children.reverse.dropWhile{n=>n.isAtom && n.text.trim.isEmpty} // get rid of trailing whitespace.
            val lhs = revkids.tail.reverse
            val rhs = revkids.head
            <table class="xsForm"><tr><td><div style="display:inline-block;white-space:nowrap;">{lhs map transform}</div></td><td class="xsErrorLabeled xsColMainWidth1">{transform(rhs)}</td></tr></table>
          case Elem("label",fieldName, attribs, scope, children @ _*) => labels.get(fieldName).getOrElse(NodeSeq.Empty)
          case Elem("labelib",fieldName, attribs, scope, children @ _*) => labels.get(fieldName) match {
            case Some(field) =>
              val width = attribs("width") match { case null => "" ; case w => "width:"+(w.text)+";" }
              val style = "display:inline-block;" + width
              <div style={style}>{field}</div>
            case None => NodeSeq.Empty
          }
          case Elem("tr",fieldName, attribs, scope, children @ _*) => trs.get(fieldName).getOrElse(NodeSeq.Empty)
          case Elem("line",fieldName, attribs, scope, children @ _*) => <table class="xsForm">{trs.get(fieldName).getOrElse(NodeSeq.Empty)}</table>
          case elem: Elem =>
            //println("Other elem : prefix = "+elem.prefix+" namespace = "+elem.namespace+" name="+elem.label)
            elem copy (child = elem.child flatMap (this.transform))
          case other => other
        }
      }
    }
    val result = (new RuleTransformer(t1))(template)
    //println("result = "+result)
    result
  }
}
