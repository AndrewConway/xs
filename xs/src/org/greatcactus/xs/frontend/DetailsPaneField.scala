/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.frontend

import org.greatcactus.xs.impl.XSFieldInfo
import org.greatcactus.xs.impl.SerializableTypeInfo
import scala.collection.mutable.ArrayBuffer
import java.util.Locale
import scala.collection.mutable.ListBuffer
import org.greatcactus.xs.impl.EnumeratedOptions
import org.greatcactus.xs.api.display.TextLocalizationResources
import org.greatcactus.xs.api.icon.Icon
import org.greatcactus.xs.api.icon.SystemIcons
import org.greatcactus.xs.impl.DependencyInjectionFunction
import org.greatcactus.xs.api.display.RichLabel
import scala.util.Success
import scala.collection.GenTraversable
import org.greatcactus.xs.impl.GeneralizedField

/**
 * Immutable description of the fields on the details pane.
 * 
 * <p>
 * Editing an XS structure consists of a pair of panes, the TreePane on the left, and the DetailsPane on the right.
 * This describes the structure of the details pain for a particular class, including resolution of all natural
 * language and division into sections. It is immutable, designed as a template for the XSDetailsPane class. 
 * </p>
 * 
 * The screen itself is composed of sections, which are in turn composed of fields. 
 * 
 * TODO The classes here could do with a refactor.
 */
class DetailsPaneFields(
    /** Virtual field representing the (optional) title */
    val field : DetailsPaneFieldSection, 
    val sections:List[DetailsPaneSection]
    ) {

  override def toString = List(field.title.toList,field.description.toList,sections).flatten.mkString("\n")
  
  def justFields : List[DetailsPaneField] = sections.flatMap(_.fields)
}

object DetailsPaneFields {
  
  val priorityDelete = 1000
  val wholeFormAsFieldName = "WholeForm";
  
  def apply(t:SerializableTypeInfo[_],locale:Locale,mayDelete:Boolean) = {
    val text = t.textResources(locale)
    val fields = new ListBuffer[EditPaneElem]
    for (f<-t.fields) { 
      if (f.isIndividuallyEditable) { // put in all the "adds"
        for (sub<-f.xsinfo.get.transitiveSubclasses) {
          val subt = sub.textResources(locale)
          val title = text.get(f.name+".Add_"+sub.name).orElse(text.get(f.name+".Add")).orElse(subt.get("Add")).getOrElse("Add "+f.name)
          val titletooltip = text.get(f.name+".Add_"+sub.name+".tooltip").orElse(text.get(f.name+".Add.tooltip")).orElse(subt.get("Add.tooltip"))
          val field = new DetailsPaneFieldActionAdd(title,titletooltip,f,sub)
          fields+=new EditPaneElem(field,f.displayOptions.editSection,f.displayOptions.orderingPriority)
        }
      } else if (f.isStringEditable) {
        val choices = for (c<-f.fixedOptions) yield { 
          val lc = for (cl<-c.classForLocalizationResources) yield TextLocalizationResources.getCached(locale, cl)
          val prefix = f.name+".enum."
          def iconFromMain(s:String) : Option[Icon] = for (logicalName<-text.get(prefix+s+".icon"); icon<-t.iconSource.iconOfLogicalName(logicalName)) yield icon
          def iconFromOther(s:String) : Option[Icon] = {
            // println("s="+s+"  lc(s.icon)="+lc.flatMap(_.get(s+".icon"))+"  lc(s_icon)="+lc.flatMap(_.get(s+"_icon"))+"  lc(s)="+lc.flatMap(_.get(s)))
            for (llc<-lc; is<-c.iconSource; logicalName<-llc.get(s+".icon"); icon<-is.iconOfLogicalName(logicalName)) yield icon 
          }
          def getIcon(s:String) : Option[Icon] = iconFromMain(s).orElse(iconFromOther(s))
          def optloc(s:String) = text.get(prefix+s).orElse(lc.flatMap{_.get(s)})
          def loc(s:String) = optloc(s).getOrElse(s) 
          new LocalizedTextChoices(c.required,for (s<-c.options) yield new LocalizedChoice(s,loc(s),getIcon(s)))
        } 
        val field = new DetailsPaneFieldText(f,text(f.name),text.get(f.name+".tooltip"),f.displayOptions.icon,f.errorIfBlank,f.displayOptions.maxLength,f.expectedRegex,f.displayOptions.displayOnly,f.displayOptions.multiline,f.displayOptions.hideName,f.displayOptions.wholeLine,text.get(f.name+".placeholder"),f.displayOptions.knownInterpretation,choices,t.dependencyInjectionInfo.fieldsThatCouldHaveErrors)
        fields+=new EditPaneElem(field,f.displayOptions.editSection,f.displayOptions.orderingPriority)
      } else if (f.isBooleanEditable) {
        val field = new DetailsPaneFieldBoolean(f,text(f.name),text.get(f.name+".tooltip"),f.displayOptions.icon,t.dependencyInjectionInfo.fieldsThatCouldHaveErrors)
        fields+=new EditPaneElem(field,f.displayOptions.editSection,f.displayOptions.orderingPriority)        
      } 
      if (f.isImageEditable) {
        val field = new DetailsPaneFieldImage(f,text(f.name),text.get(f.name+".tooltip"),f.displayOptions.icon,t.dependencyInjectionInfo.fieldsThatCouldHaveErrors)
        fields+=new EditPaneElem(field,f.displayOptions.editSection,f.displayOptions.orderingPriority)                
      } else if (f.isTableEditable) {
        val columns: List[DetailsPaneField] = f.xsinfo.get.getPane(locale,false).justFields
        for (c<-columns) if (!c.validInTable) throw new IllegalArgumentException("Invalid column "+c.name+" in table")
        val field = new DetailsPaneFieldTable(f,text(f.name),text.get(f.name+".tooltip"),f.displayOptions.icon,columns,locale,t.dependencyInjectionInfo.fieldsThatCouldHaveErrors)
        fields+=new EditPaneElem(field,f.displayOptions.editSection,f.displayOptions.orderingPriority)                
      }
    }
    for (f<-t.dependencyInjectionInfo.extraText) { // put in the (read only) pseudo text fields
      val field = new DetailsPaneFieldShowText(f.function,text(f.name),text.get(f.name+".tooltip"),f.displayOptions.icon,f.displayOptions.multiline,f.displayOptions.hideName,f.displayOptions.wholeLine,t.dependencyInjectionInfo.fieldsThatCouldHaveErrors)
        fields+=new EditPaneElem(field,f.displayOptions.editSection,f.displayOptions.orderingPriority)
    }
    if (mayDelete) { // put in (if relevent) the "delete"
      val field = new DetailsPaneFieldActionDelete(text("Delete"),text.get("Delete.Tooltip"))
      fields+=new EditPaneElem(field,None,priorityDelete)
    }
    def getSectionField(name:String) = {
      val title = text.get(name)
      val description = text.get(name+".details")
      val icon : Option[Icon] = for (logicalName<-text.get(name+".icon"); icon<-t.iconSource.iconOfLogicalName(logicalName)) yield icon
      new DetailsPaneFieldSection(name,title,description,text.get(name+".tooltip"),icon,t.dependencyInjectionInfo.fieldsThatCouldHaveErrors)      
    }
    val sections = for ((sectionName,elems)<-fields.toList.groupBy{_.sectionName}) yield {
      val name = "Section."+sectionName.getOrElse("Main")
      //val couldContainError = t.dependencyInjectionInfo.fieldsThatCouldHaveErrors.contains(name)
      val sortedElems = elems.sortBy(_.priority)
      new DetailsPaneSection(getSectionField(name),sortedElems.map{_.field})
    }
    new DetailsPaneFields(getSectionField(wholeFormAsFieldName),sections.toList)
  }
  
  private class EditPaneElem(val field:DetailsPaneField,val sectionName:Option[String],val priority:Int)
}

class DetailsPaneSection(
    /** Computer readable. Generally ignored by client. */
    val field : DetailsPaneFieldSection,
    /*
    val name:String,
    val field
    val couldContainErrorIcon : Boolean,
    /** Human readable */
    val title : Option[String],
    val description : Option[String], */
    val fields : List[DetailsPaneField]
    ) {
  override def toString = List(field.title.toList,field.description.toList,fields).flatten.mkString("\n")
}
    
/**
 * The main thing that fields are composed of.    
 */
sealed abstract class DetailsPaneField {
  def name:String
  def couldContainErrorIcon : Boolean
  def label:String
  def icon:Option[Icon]
  def shouldBeEnabled(parent:XSTreeNode) = parent.isEnabled(name)
  def shouldBeVisible(parent:XSTreeNode) = parent.isVisible(name)
  /** None if this type of field is not valid in a table. Otherwise either a field or a dependency function */
  def columnExtractor : Option[GeneralizedField] 
  def validInTable : Boolean = columnExtractor.isDefined
}

trait DetailsPaneFieldBasedOnSimpleField {
  def field:XSFieldInfo
  def name=field.name
}

class DetailsPaneFieldSection(
    val name:String,
    val title:Option[String],
    val description:Option[String],
    /** For the title */
    val tooltip:Option[String],
    val icon:Option[Icon],
    fieldsThatCouldHaveErrors:Set[String]
    ) extends DetailsPaneField {
  def label : String = title.getOrElse(null) 
  val couldContainErrorIcon : Boolean = fieldsThatCouldHaveErrors.contains(name)
  override def columnExtractor =None
  override def toString = title.getOrElse(name)
}



sealed abstract class DetailsPaneFieldAction extends DetailsPaneField {
  def go(edit:XSEdit,parent:XSTreeNode)
  def label:String
  def tooltip:Option[String]
  def icon:Option[Icon]
  override def toString = label
  def couldContainErrorIcon : Boolean = false
  override def columnExtractor =None
}
// class SimpleAction(val description:String,val tooltip:Option[String]) extends PaneField

class DetailsPaneFieldActionAdd(
    val label:String,
    val tooltip:Option[String],
    val field:XSFieldInfo,
    val concreteClass:SerializableTypeInfo[_]
    ) extends DetailsPaneFieldAction {
  def icon = concreteClass.icon
  def go(edit:XSEdit,parent:XSTreeNode) {
    edit.addField(parent,None, field,concreteClass.newElement().asInstanceOf[AnyRef])
  }
  override def shouldBeVisible(parent:XSTreeNode) = parent.canAdd(field) && parent.isVisible(name)
  val name = "add."+concreteClass.name
}

class DetailsPaneFieldActionDelete(
    val label:String,
    val tooltip:Option[String]
    ) extends DetailsPaneFieldAction {
  def go(edit:XSEdit,nodeBeingDeleted:XSTreeNode) {
    edit.deleteTreeNode(nodeBeingDeleted)
  }
  def icon = Some(SystemIcons.delete)
  def name = "delete"
}

sealed abstract class DetailsPaneFieldLabeled extends DetailsPaneField {
    def label:String
    def tooltip:Option[String]
    def icon:Option[Icon]
    def multiline:Boolean
    def hideName:Boolean
    def wholeLine:Boolean
}

class DetailsPaneFieldShowText(val function:DependencyInjectionFunction,val label:String,val tooltip:Option[String],val icon:Option[Icon],val multiline:Boolean,val hideName:Boolean,val wholeLine:Boolean,fieldsThatCouldHaveErrors:Set[String]) extends DetailsPaneFieldLabeled {
  def get(parent:XSTreeNode,locale:Locale) : RichLabel = parent.getPseudoField(function,locale)
  def name = function.name
  override def toString = label
  val couldContainErrorIcon : Boolean = fieldsThatCouldHaveErrors.contains(name)
  override def columnExtractor = Some(function)
}

class DetailsPaneFieldBoolean(val field:XSFieldInfo,val label:String,val tooltip:Option[String],val icon:Option[Icon],fieldsThatCouldHaveErrors:Set[String]) extends DetailsPaneFieldLabeled with DetailsPaneFieldBasedOnSimpleField {
  def get(parent:XSTreeNode) : Boolean = field.getField(parent.getObject).asInstanceOf[Boolean]
  def set(edit:XSEdit,parent:XSTreeNode,newValue:Boolean) { 
    edit.setField(parent, field, newValue.asInstanceOf[AnyRef],None)
  }
  override def toString = label
  val couldContainErrorIcon : Boolean = fieldsThatCouldHaveErrors.contains(name)
  override def multiline = false
  override def hideName = false
  override def wholeLine = false
  override def columnExtractor = Some(field)
}

class DetailsPaneFieldImage(val field:XSFieldInfo,val label:String,val tooltip:Option[String],val icon:Option[Icon],fieldsThatCouldHaveErrors:Set[String]) extends DetailsPaneFieldLabeled with DetailsPaneFieldBasedOnSimpleField {
  def resolveNetworkReferences = field.resolveNetworkReferences
  def get(parent:XSTreeNode) : String = field.getFieldAsString(parent.getObject)
  def set(edit:XSEdit,parent:XSTreeNode,stringrep:String) : Boolean = { 
    println("Setting image to "+stringrep)
    // TODO resolve network references which may or may not be doable on the client. Not much point doing it yet as the client code is not that robust yet.
    field.parseStringPossiblyMultipleSafe(stringrep) match {
      case Success(newValue) => edit.setField(parent, field, newValue,None); true
      case _ => false
    }
  }
  override def toString = label
  val couldContainErrorIcon : Boolean = fieldsThatCouldHaveErrors.contains(name)
  override def multiline = field.displayOptions.multiline
  override def hideName = field.displayOptions.hideName
  override def wholeLine = field.displayOptions.wholeLine
  override def columnExtractor = Some(field)
}

class DetailsPaneFieldTable(val field:XSFieldInfo,val label:String,val tooltip:Option[String],val icon:Option[Icon],val columns: List[DetailsPaneField],val locale:Locale,fieldsThatCouldHaveErrors:Set[String]) extends DetailsPaneFieldLabeled with DetailsPaneFieldBasedOnSimpleField {
  def resolveNetworkReferences = field.resolveNetworkReferences
  def getNodes(parent:XSTreeNode) : IndexedSeq[XSTreeNode] = parent.tableChildren(field)
  def get(parent:XSTreeNode) : (IndexedSeq[XSTreeNode],IndexedSeq[IndexedSeq[String]]) = {
    val rows = parent.tableChildren(field)
    val data = rows.map{_.getTableFields(columnExtractors)}
    (rows,data)
  }
  
  override def toString = label
  val couldContainErrorIcon : Boolean = fieldsThatCouldHaveErrors.contains(name)
  val columnExtractors = new ColumnExtractors(columns.flatMap{_.columnExtractor}(collection.breakOut),locale)
  override def multiline = true
  override def hideName = field.displayOptions.hideName
  override def wholeLine = field.displayOptions.wholeLine
  override def columnExtractor = None
}

class ColumnExtractors(val fields:IndexedSeq[GeneralizedField],val locale:Locale) {
  def extract(parent:XSTreeNode) : IndexedSeq[String] = for (f<-fields) yield f match {
    case field:XSFieldInfo => field.getFieldAsString(parent.getObject)
    case dep:DependencyInjectionFunction => parent.getPseudoField(dep, locale).html.toString
    case _ => throw new IllegalArgumentException(f.toString)
  }
  
  override val hashCode = fields.hashCode+locale.hashCode*7
  override def equals(o:Any) = o match {
    case other:ColumnExtractors => (this eq other) || (fields==other.fields && locale==other.locale)
    case _ => false
  }
  def indexOfFieldNamed(name:String) : Int = fields.indexWhere(_.name==name)
  def xsFieldNamed(name:String): (XSFieldInfo,Int) = {
    val ind = indexOfFieldNamed(name)
    if (ind== -1) throw new IllegalArgumentException("Unknown field name "+name)
    fields(ind) match {
      case field:XSFieldInfo => (field,ind)
      case _ => throw new IllegalArgumentException("Field "+name+" is not mutable")
    }
  }
  def length : Int = fields.length
}

class DetailsPaneFieldText(
    val field:XSFieldInfo,
    val label:String,
    val tooltip:Option[String],
    val icon:Option[Icon],
    val required:Boolean,
    val maxlength:Option[Int],
    val regex:Option[String], // "pattern" in HTML
    val readonly:Boolean,
    val multiline:Boolean,
    val hideName:Boolean,
    val wholeLine:Boolean,
    val placeholder:Option[String], // "placeholder" in HTML5
    val knownType:Option[String],   // known types. May or may not be supported by the UI. HTML5 ones include date, email, password, tel, time
    val choices:Option[LocalizedTextChoices],
    fieldsThatCouldHaveErrors:Set[String]
    ) extends DetailsPaneFieldLabeled with DetailsPaneFieldBasedOnSimpleField {
  def get(parent:XSTreeNode) : String = field.getFieldAsString(parent.getObject)
  /** Return true if the new string value is legal */
  def set(parent:XSTreeNode,stringrep:String,executeAfterModificationBeforeRefreshing:Option[()=>Unit]) : Boolean = { 
    field.parseStringPossiblyMultipleSafe(stringrep) match {
      case Success(newValue) => parent.xsedit.setField(parent, field, newValue,executeAfterModificationBeforeRefreshing); true
      case _ => false
    }
  }
  override def toString = label
  def ok(s:String) = {
    if (s==null || s.trim.isEmpty) !required
    else if (maxlength.isDefined && s.length>maxlength.get) false
    else if (regex.isDefined && ("^"+regex.get+"$").r.findAllMatchIn(s).isEmpty) false
    else true
  }
  val couldContainErrorIcon : Boolean = fieldsThatCouldHaveErrors.contains(name)
  override def columnExtractor = Some(field)
}

class LocalizedChoice(val original:String,val localized:String,val icon:Option[Icon])
class LocalizedTextChoices(val required:Boolean,val choices:Seq[LocalizedChoice]) {
  val lookupOriginal : Map[String,LocalizedChoice] = Map.empty++(for (c<-choices) yield c.original->c)
  def lookupOriginalNull(original:String) = if (original==null) null else lookupOriginal.get(original).getOrElse(null)
}
