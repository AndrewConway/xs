/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.test

import org.junit.Assert._
import org.junit.Test
import scala.annotation.ClassfileAnnotation
import scala.reflect.ClassTag
import org.greatcactus.xs.impl.SerializableTypeInfo
import org.greatcactus.xs.api.XS
import org.greatcactus.xs.api.XSSubclasses
import org.greatcactus.xs.impl.NotSerializableException
import org.greatcactus.xs.impl.XSSpecificationError
import org.greatcactus.xs.frontend._
import java.util.Locale
import org.greatcactus.xs.api.display.RichLabel
import org.greatcactus.xs.api.icon.Icon
import org.greatcactus.xs.api.errors.ResolvedXSError



 /**
 * Test the Space classes editing.
 */
class ZZZ_EditSpace {

  
  
  @Test
  def test {
    val base = new Space(new History(""),Nil)
    val edit = new XSEdit(base,Some(SpaceExternalDependencyResolver))
    val details = new TestXSDetailsPane(Locale.FRANCE,edit)
    edit.addDetailsPane(details)
    //println(edit.toString)
    assertEquals("*- Space\n  . History",edit.toString)
    assertEquals("Edit Space\nSpace is big. This contains the things in space (that is, everything)\nAdd history\nAjoutez un étoile",details.getCurrentPane.get.toString)
    details.uiActivated("Ajoutez un étoile")
    assertEquals(" - Space\n  . History\n* . New star",edit.toString)
    assertEquals("Edit Star\nInformation about a stellar system\nName\nconstellation\nAjoutez un étoile\nAdd Planet\nAdd Comet\nconstellationRaw\nName backwards\nDelete Star",details.getCurrentPane.get.toString)
    details.uiChangedTextField("name","Sol",true)
    assertEquals(" - Space\n  . History\n* . Sol",edit.toString)
    details.uiActivated("Add Planet")
    assertEquals(" - Space\n  . History\n  - Sol\n*  . New planet",edit.toString)
    assertEquals("Planetary system components\nThe things in the planetary system\nName\nDistance from primary\nType\nAdd Core\nAdd moon\nTidally Locked\nphoto\nDelete",details.getCurrentPane.get.toString)    
    details.uiChangedTextField("name","Mercury",true)
    assertEquals(" - Space\n  . History\n  - Sol\n*  . Mercury",edit.toString)
    details.uiChangedTextField("distanceFromPrimary","0.38",true)
    assertEquals(" - Space\n  . History\n  - Sol\n*  . Mercury",edit.toString)
    assertEquals("Planetary system components\nSection.Main\nName:Mercury\nDistance from primary:0.38\nType:null\nAdd Core\nAdd moon\nTidally Locked:false\nphoto:null\nDelete",details.getCurrentUIElements.get.toString)  
    edit.changeCurrentlyEditing(edit.currentlyEditing.parent)
    assertEquals(" - Space\n  . History\n* - Sol\n   . Mercury",edit.toString)
    details.uiActivated("Add Planet")
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n*  . New planet",edit.toString)
    details.uiChangedTextField("name","Venus",true)
    details.uiChangedTextField("distanceFromPrimary","0.72",true)
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n*  . Venus",edit.toString)
    edit.changeCurrentlyEditing(edit.currentlyEditing.parent)
    details.uiActivated("Add Planet")
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n   . Venus\n*  . New planet",edit.toString)
    details.uiChangedTextField("name","Earth",true)
    details.uiChangedTextField("distanceFromPrimary","1",true)
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n   . Venus\n*  . Earth",edit.toString)
    details.uiActivated("Add moon")
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n   . Venus\n   - Earth (1 moon)\n*   . New planet",edit.toString)
    details.uiChangedTextField("name","Luna",true)
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n   . Venus\n   - Earth (1 moon)\n*   . Luna",edit.toString)
    edit.changeCurrentlyEditing(edit.currentlyEditing.parent.parent)
    details.uiActivated("Add Planet")
    details.uiChangedTextField("name","Pluto",true)
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n   . Venus\n   - Earth (1 moon)\n    . Luna\n*  . Pluto",edit.toString)
    details.uiActivated("Delete") // Sorry Pluto, you;ve been pluto'd
    assertEquals(" - Space\n  . History\n* - Sol\n   . Mercury\n   . Venus\n   - Earth (1 moon)\n    . Luna",edit.toString)
    // close Earth and Venus
    edit.currentlyEditing.treeChildren(1).isOpen=false
    edit.currentlyEditing.treeChildren(2).isOpen=false
    assertEquals(" - Space\n  . History\n* - Sol\n   . Mercury\n   . Venus\n   + Earth (1 moon)",edit.toString)
    // copy Venus
    val sol = edit.currentlyEditing
    val venusCopy = edit.copyData(List(sol.treeChildren(1)))
    val venusCopyS = new String(venusCopy,"UTF-8")
    println(venusCopyS)
    assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<xs-copied-data><Planet name=\"Venus\" distanceFromPrimary=\"0.72\" tidallyLocked=\"false\"/></xs-copied-data>",venusCopyS)
    edit.pasteData(sol,venusCopy,None)
    assertEquals(" - Space\n  . History\n* - Sol\n   . Mercury\n   . Venus\n   + Earth (1 moon)\n   . Venus",edit.toString)
    // Copy Venus and Earth
    val venusEarthCopy = edit.copyData(List(sol.treeChildren(1),sol.treeChildren(2)))
    val venusEarthCopyS = new String(venusEarthCopy,"UTF-8")
    assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<xs-copied-data><Planet name=\"Venus\" distanceFromPrimary=\"0.72\" tidallyLocked=\"false\"/><Planet name=\"Earth\" distanceFromPrimary=\"1.0\" tidallyLocked=\"false\"><Planet name=\"Luna\" distanceFromPrimary=\"0.0\" tidallyLocked=\"false\"/></Planet></xs-copied-data>",venusEarthCopyS)
    edit.pasteData(sol,venusEarthCopy,None)
    assertEquals(" - Space\n  . History\n* - Sol\n   . Mercury\n   . Venus\n   + Earth (1 moon)\n   . Venus\n   . Venus\n   + Earth (1 moon)",edit.toString)
    edit.changeCurrentlyEditing(sol.treeChildren(3))
    details.uiChangedTextField("name","Mars",true)
    edit.changeCurrentlyEditing(sol.treeChildren(4))
    details.uiChangedTextField("name","Jupiter",true)
    edit.changeCurrentlyEditing(sol.treeChildren(5))
    details.uiChangedTextField("name","Saturn",true)
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n   . Venus\n   + Earth (1 moon)\n   . Mars\n   . Jupiter\n*  + Saturn (1 moon)",edit.toString)
    edit.pasteData(sol,venusCopy,Some(sol.treeChildren(4)))
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n   . Venus\n   + Earth (1 moon)\n   . Mars\n   . Venus\n   . Jupiter\n*  + Saturn (1 moon)",edit.toString)    
    edit.changeCurrentlyEditing(sol.treeChildren(4))
    details.uiChangedTextField("name","Asteroid Belt",true)
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n   . Venus\n   + Earth (1 moon)\n   . Mars\n*  . Asteroid Belt\n   . Jupiter\n   + Saturn (1 moon)",edit.toString)  
    // move Venus and Mars to the end
    edit.dragData(sol,List(sol.treeChildren(1),sol.treeChildren(3)),None)
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n   + Earth (1 moon)\n*  . Asteroid Belt\n   . Jupiter\n   + Saturn (1 moon)\n   . Venus\n   . Mars",edit.toString)  
    // drag Venus, Jupiter, Mars to before Jupiter
    edit.dragData(sol,List(sol.treeChildren(5),sol.treeChildren(3),sol.treeChildren(6)),Some(sol.treeChildren(3)))
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n   + Earth (1 moon)\n*  . Asteroid Belt\n   . Venus\n   . Jupiter\n   . Mars\n   + Saturn (1 moon)",edit.toString)  
    edit.dragData(sol,List(sol.treeChildren(3)),Some(sol.treeChildren(1)))
    edit.dragData(sol,List(sol.treeChildren(5)),Some(sol.treeChildren(3)))
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n   . Venus\n   + Earth (1 moon)\n   . Mars\n*  . Asteroid Belt\n   . Jupiter\n   + Saturn (1 moon)",edit.toString)
    edit.changeCurrentlyEditing(sol)
    details.uiActivated("Add Comet")
    assertEquals(" - Space\n  . History\n  - Sol\n   . Mercury\n   . Venus\n   + Earth (1 moon)\n   . Mars\n   . Asteroid Belt\n   . Jupiter\n   + Saturn (1 moon)\n*  . New comet",edit.toString)
    assertEquals("Edit Comet\nInformation about a comet\nNomenclature\nSome comets have names\nName\nperiod\nDelete Comet",details.getCurrentPane.get.toString)
    println(edit.toString)
    println(details.getCurrentPane.get.toString)
    println(details.getCurrentUIElements.get.toString)
    // add a new star
    
    
    
  }

}


class TestXSDetailsPane(_locale:Locale,_xsedit:XSEdit) extends XSDetailsPane[String](_locale,_xsedit) {
  
  // things it has to implement
  def dispose(guis:UIFields) {}
  def setBlankScreen() {}
  def flushClientCommands() {}
  /** Create a new object for generating the GUI elements for a new screen */  
  def newCreator() = new TestGUICreator
  /** XS sending a command to the GUI to change what it is showing. */ 
  def changeUITextField(gui:String,shouldBe:String) {}
  def changeUIImageField(gui:String,shouldBe:String) {}
  def changeUIvisibility(gui:String,visible:Boolean) {}
  def changeUIenabledness(gui:String,enabled:Boolean) {}
  def changeUIShowText(gui:String,shouldBe:RichLabel) {}
  def changeUILabelText(gui:String,shouldBe:RichLabel) {}
  def changeUILabelIcon(gui:String,icon:Option[Icon]) {}
  def changeUIBooleanField(id:String,shouldBe:Boolean) { }
  def changeErrors(gui:String,errors:List[ResolvedXSError]) {}
  def changeGridErrors(gui:String,row:Int,col:Int,errors:List[ResolvedXSError]) {}
  def setUIFieldIllegalContents(gui:String,isIllegal:Boolean) {}
  def changeUIWholeTable(gui:String,shouldBe:IndexedSeq[IndexedSeq[String]]) {} 
  def changeUISingleLineTable(gui:String,index:Int,shouldBe:IndexedSeq[String]) {} 
  def setUITableEntriesIllegalContents(gui:String,illegalEntries:Map[Int,List[Int]]) {}

}

/** Dummy GUI creator referring to everything by its name */
class TestGUICreator extends GUICreator[String] {
  override def startForm(title:DetailsPaneFieldSection,currently:CurrentFieldState) : String =  title.name
  def startSection(section:DetailsPaneFieldSection,currently:CurrentFieldState) = section.name
  def endSection(section:DetailsPaneFieldSection,id:String,currently:CurrentFieldState) {}
  def endForm() {}
  def createAction(field:DetailsPaneFieldAction,currently:CurrentFieldState) = field.label
  def createTextField(field:DetailsPaneFieldText,currently:CurrentFieldState,initialValue:String) = field.name
  def createImageField(field:DetailsPaneFieldImage,currently:CurrentFieldState,initialValue:String) = field.name
  def createShowTextField(field:DetailsPaneFieldShowText,currently:CurrentFieldState,initialValue:RichLabel) = field.name
  def createBooleanField(field:DetailsPaneFieldBoolean,currently:CurrentFieldState,initialValue:Boolean) = field.name
  def createTableField(field:DetailsPaneFieldTable,currently:CurrentFieldState,initialValue:IndexedSeq[IndexedSeq[String]]) = field.name
}