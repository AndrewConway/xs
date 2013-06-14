/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.frontend.swing

import java.util.Locale
import scala.collection.mutable.ListBuffer
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Label
import java.awt.Font
import scala.swing.Action
import scala.swing.Button
import scala.swing.Component
import scala.swing.FlowPanel
import scala.swing.event.EditDone
import scala.swing.event.ValueChanged
import scala.swing.BorderPanel
import scala.swing.Swing
import java.awt.Dimension
import scala.swing.event.SelectionChanged
import org.greatcactus.xs.api.display.RichLabel
import scala.swing.CheckBox
import scala.swing.event.ButtonClicked
import org.greatcactus.xs.api.errors.ResolvedXSError
import org.greatcactus.xs.frontend._
import org.greatcactus.xs.impl.GeneralizedField
/**
 * XSDetailsPane specialized for Swing client
 *
 * The type of a GUI object is a string representing the base of the identifier for the corresponding gui object.
 */
class SwingDetailsPane(_locale:Locale,_xsedit:XSEdit) extends XSDetailsPane[Component](_locale,_xsedit) {
  
  val panel = new BoxPanel(Orientation.Vertical)
  
  def dispose(guis:UIFields) {
    extraSetVisibility=Map.empty
    panel.contents.clear()
  } // Do I have to dispose anything?????
  override def remove(gui:Component) {}
  
  override def setBlankScreen() { } // setNewPanel(new BoxPanel(Orientation.Vertical))}
  
  def flushClientCommands() {}

  def newCreator() = new GUICreatorSwing(this)

  override def changeUITextField(id:Component,shouldBe:String) { id match {
    case t:scala.swing.TextField => t.text=shouldBe // ; println("Changed text to "+shouldBe)
    case cb:scala.swing.ComboBox[_] => uiField(id) match {
      case Some(uit:UIFieldText) if uit.choices.isDefined => cb.asInstanceOf[scala.swing.ComboBox[LocalizedChoice]].selection.item = uit.choices.get.lookupOriginalNull(shouldBe)
      case _ =>
    }
    case _ => println("Did not find text field") // should never happen
  }}
  override def changeUIBooleanField(id:Component,shouldBe:Boolean) { id match {
    case t:scala.swing.CheckBox => t.selected=shouldBe // ; println("Changed text to "+shouldBe)
    case _ => println("Did not find check box") // should never happen
  }}
  override def changeUIShowText(id:Component,shouldBe:RichLabel) { id match {
    case t:scala.swing.Label => t.text="<html>"+shouldBe.html.toString+"</html>" // ; println("Changed text to "+shouldBe)
    case _ => println("Did not find label") // should never happen    
  }}

  override def changeUIvisibility(id:Component,visible:Boolean) {
    extraSetVisibility.get(id).getOrElse(id).visible=visible; 
  } 
  override def changeUIenabledness(id:Component,enabled:Boolean) {
    //println("Enabled = "+enabled)
    id.enabled=enabled 
  }
  
  var extraSetVisibility : Map[Component,Component] = Map.empty // somewhat of a hack so that we can hide/show both the label and field
  
  def setNewPanel(newpanel:BoxPanel) {
    panel.contents+=newpanel
    panel.revalidate()
    panel.repaint()
    // do I have to repack
  }
  override def changeUILabelText(gui:Component,shouldBe:RichLabel) {} // TODO also handle on creation
  override def changeUILabelIcon(gui:Component,icon:Option[org.greatcactus.xs.api.icon.Icon]) {} // TODO also handle on creation
  override def changeErrors(gui:Component,errors:List[ResolvedXSError]) {} // TODO
  override def changeGridErrors(gui:Component,row:Int,col:Int,colfield:GeneralizedField,errors:List[ResolvedXSError]) {} // TODO
  override def setUIFieldIllegalContents(gui:Component,isIllegal:Boolean) {} // TODO
  override def changeUIImageField(gui:Component,shouldBe:String) {} // TODO
  override def changeUIWholeTable(gui:Component,shouldBe:IndexedSeq[IndexedSeq[String]]) {} // TODO
  override def changeUISingleLineTable(gui:Component,index:Int,shouldBe:IndexedSeq[String]) {} // TODO
  override def setUITableEntriesIllegalContents(gui:Component,illegalEntries:Map[Int,List[Int]]) {} // TODO
  override def initiatePopup(field:UIFieldText,popupName:String,node:XSTreeNode) {} // TODO
  override def changeGridTooltip(gui:Component,row:Int,col:Int,colfield:GeneralizedField,tooltip:Option[RichLabel]) {}
  override def changeUITooltip(gui:Component,tooltip:Option[RichLabel]) {}

  def getCustom(f:DetailsPaneFieldCustom) : Option[CustomComponent[_,Component]] = SwingDetailsPane.getCustom(f)

}

object SwingDetailsPane extends CustomComponentStore[Component]

/** 
 * A GUI creator for Swing, where the definer of an object is its component. 
 * */
class GUICreatorSwing(pane:SwingDetailsPane) extends GUICreator[Component] {
  
  val panel = new BoxPanel(Orientation.Vertical)
    
  private def addLabel(labeltext:String,fontmod:Int) {
      val l = new Label { text = labeltext }
      val font = l.peer.getFont()
      if (font!=null) {
        l.peer.setFont(font.deriveFont(fontmod))      
      }
      panel.contents+=l    
  } 
  
  override def startForm(title:DetailsPaneFieldSection,currently:CurrentFieldState,template:Option[xml.Node]) = {
    for (t<-title.title) addLabel(t,Font.BOLD)
    for (t<-title.description) addLabel(t,Font.ITALIC)
    panel
  }
  
  override def startSection(section:DetailsPaneFieldSection,currently:CurrentFieldState) = {
    for (t<-section.title) addLabel(t,Font.BOLD)
    for (t<-section.description) addLabel(t,Font.ITALIC)
    panel // TODO return something more appropriate.
  }
  
  def endSection(section:DetailsPaneFieldSection,id:Component,currently:CurrentFieldState) { }
  
  
  override def endForm() = {
    panel.contents+=Swing.VGlue
    pane.setNewPanel(panel)
    panel
  }
  def createAction(field:DetailsPaneFieldAction,currently:CurrentFieldState) : Button = {
    val link = new Button;
    val action = Action(field.label) {
        //println("Activated "+field.description)
        pane.uiActivated(link) 
    } 
    link.action=action
    link.enabled=currently.enabled
    link.visible=currently.visible
    panel.contents+=link
    link
  }

  def createBooleanField(field:DetailsPaneFieldBoolean,currently:CurrentFieldState,initialValue:Boolean) : CheckBox = {
    val cb = new CheckBox(field.label);
    cb.selected=initialValue;
    cb.listenTo(cb)
    cb.reactions+={
      case ButtonClicked(`cb`) =>
        val newState = cb.selected
        pane.uiChangedBooleanField(cb,newState)
    }
    cb.enabled=currently.enabled
    cb.visible=currently.visible
    panel.contents+=cb
    cb
  }
  def createInlineField(field:DetailsPaneFieldInline,currently:CurrentFieldState) : (scala.swing.Component,GUICreator[scala.swing.Component]) = ???

  
  def createTextField(field:DetailsPaneFieldText,currently:CurrentFieldState,initialValue:String) : scala.swing.Component = {
    val gui = field.choices match {
      case Some(choices) if choices.required =>
        val cb = new scala.swing.ComboBox(choices.choices)
        cb.renderer = scala.swing.ListView.Renderer(c=>if (c==null) "" else c.localized)
        cb.listenTo(cb.selection)
        cb.reactions+={
          case SelectionChanged(`cb`) =>
            val item = cb.selection.item
            pane.uiChangedTextField(cb,if (item==null) null else item.original, true)
        }
        val item = choices.lookupOriginalNull(initialValue)
        cb.selection.item=item
        cb
      case None =>
        val tf = if (field.multiline) new scala.swing.TextArea else new scala.swing.TextField
        for (ml<-field.maxlength) tf.columns=ml
        tf.editable= !field.readonly
        tf.text=initialValue
        tf match { case rtf:scala.swing.TextField => rtf.verifier=field.ok(_); case _ => } // should have a TextArea verifier...
        tf.listenTo(tf)
        tf.reactions+={
          case EditDone(_) => pane.uiChangedTextField(tf, tf.text, true)
          case ValueChanged(_) => pane.uiChangedTextField(tf, tf.text, false)
        }
        tf
    }
    createLabeled(gui,field,currently)
    gui
  }
  
  def createImageField(field:DetailsPaneFieldImage,currently:CurrentFieldState,initialValue:String) : scala.swing.Component = ??? // TODO implement
  def createTableField(field:DetailsPaneFieldTable,currently:CurrentFieldState,initialValue:IndexedSeq[IndexedSeq[String]]) = ??? // TODO implement
  override def createCustom[S](field:DetailsPaneFieldCustom,custom:CustomComponent[S,Component],currently:CurrentFieldState,initialValue:S) = ??? // TODO implement
  
  def createShowTextField(field:DetailsPaneFieldShowText,currently:CurrentFieldState,initialValue:RichLabel) : scala.swing.Component = {
    val gui = new Label("<html>"+initialValue.html.toString+"</html>")
    createLabeled(gui,field,currently)
    gui
  }
  
  def createLabeled(gui:Component,field:DetailsPaneFieldLabeled,currently:CurrentFieldState) {
    gui.enabled=currently.enabled
    gui.maximumSize=new Dimension(Integer.MAX_VALUE,gui.preferredSize.height)
    //println("Pref = "+gui.preferredSize)
    //println("Max = "+gui.maximumSize)
    val withlabel = if (field.hideName) gui else {
      val label = new Label(field.label)
      //for (i<-field.icon) label.icon=i // TODO
      for (t<-field.tooltip) label.tooltip=t
      val subpanel = new BorderPanel {
        add(label,if (field.wholeLine) BorderPanel.Position.North else BorderPanel.Position.West)
        add(gui,BorderPanel.Position.Center)
      }
      subpanel.maximumSize=new Dimension(Integer.MAX_VALUE,gui.preferredSize.height+(if (field.wholeLine) label.preferredSize.height else 0))
      subpanel        
    }
    withlabel.visible=currently.visible 
    panel.contents+=withlabel
    pane.extraSetVisibility+= gui->withlabel    
  }

}


