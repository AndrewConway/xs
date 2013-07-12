/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.impl

import scala.reflect.runtime._
import scala.reflect.runtime.universe._
import org.greatcactus.xs.api.serialization._
import org.greatcactus.xs.api._
import scala.collection.GenTraversable
import java.util.Arrays
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer
import javax.xml.stream.XMLStreamReader
import java.util.Locale
import org.greatcactus.xs.api.errors._
import org.greatcactus.xs.frontend.DetailsPaneFields
import org.greatcactus.xs.api.icon.IconManifests
import org.greatcactus.xs.api.icon.Icon
import scala.collection.mutable.ListBuffer
import org.greatcactus.xs.api.dependency._
import org.greatcactus.xs.api.edit._
import org.greatcactus.xs.api.display._
import org.greatcactus.xs.util.EqualityByPointerEquality
import org.greatcactus.xs.api.command.XSCommand
import scala.xml.NodeSeq
import scala.xml.XML


/**
 * Information on a type that is being serialized
 */
class SerializableTypeInfo[T <: AnyRef] private (val clazz : java.lang.Class[T]) extends TextLocalizationSource {

  import SerializableTypeInfo._
  val rootMirror =  universe.runtimeMirror(clazz.getClassLoader)
  val classSymbol = rootMirror.classSymbol(clazz)
  val classMirror = rootMirror.reflectClass(classSymbol)
  val ty = classSymbol.toType
  def error(cause:String) = throw new XSSpecificationError(clazz,cause)
  def isAbstract = classSymbol.isAbstractClass
  
  val iconSource = IconManifests.getIconManifests(clazz)
  
  val icon : Option[Icon] = for (logicalName<-getOptionalValue(classSymbol,typeXSIcon); if logicalName!=null && !logicalName.isEmpty(); icon<-iconSource.iconOfLogicalName(logicalName)) yield icon
  
  if (!hasAnnotation(classSymbol,typeXSSerializable)) throw new NotSerializableException
  
  private val overridingName = getOptionalValue(classSymbol,typeXSName)
  
  /** The name used for serializing this class */
  val name = overridingName.getOrElse(classSymbol.name.decoded)
  
  /** Subclasses mentioned in SerializableSubclasses structure */
  val subclasses : Array[SerializableTypeInfo[_]] = {
    val ca = clazz.getAnnotation(classOf[XSSubclasses])
    if (ca==null) new Array[SerializableTypeInfo[_]](0) else {
      for (c<-ca.value) yield SerializableTypeInfo.get(c) match {
        case Some(x) => x
        case None => error("Subclass "+c.getName+" is not XS serializable.")
      }
    } 
  }
  /** transitive completion of subclasses mentioned in SerializableSubclasses structure including self but not including abstract classes. */
  private[xs] lazy val transitiveSubclasses : Seq[SerializableTypeInfo[_ <: AnyRef]] = (subclasses.flatMap{_.transitiveSubclasses}++List(this)).filter{!_.isAbstract}
    
  lazy val subClassFromName : Map[String,SerializableTypeInfo[_ <: T]] = Map.empty++transitiveSubclasses.map{s=>s.name->s.asInstanceOf[SerializableTypeInfo[_ <: T]]}

  /**
   * This (and related) has to be lazy as it may include a reference to itself, which would require the class itself to be constructed. 
   * However, until it is instantiated we can't tell whether this class is valid. So we have method checkIsValid below.
   */
  private lazy val constructor : Option[ConstructorInfo] = {
    val constructors = ty.member(universe.nme.CONSTRUCTOR)
    if (constructors.isTerm) Some(new ConstructorInfo(constructors.asTerm))
    else None
  }
  private var checkedValid = false
  private var checkedError : Option[Exception] = None
  
  def checkValid() {
    syncronizeValidityChecks.synchronized {
      if (checkedValid) checkedError.foreach(throw _)
      else try {
        checkedValid=true;
        constructor match {
          case Some(c) =>
            for (f<-c.fieldsAsBlocks; info<-f.xsinfo) info.checkValid()
          case None =>
        }         
      } catch { case e:Exception => checkedError=Some(e); throw e }
    }
  }
  
  /** Information gleaned from the constructor, which may not exist if the class is abstract. */
  class ConstructorInfo(constructors:universe.TermSymbol) {
    private val constructor = {
      if (constructors.alternatives.length==1) constructors.asMethod 
      else {
        val filtered = constructors.alternatives.filter{hasAnnotation(_,typeXSConstructor)}
        if (filtered.length==1) filtered.head.asMethod
        else error("No unique constructor annotated with XSConstructor")
      }
    }
    private val reflectedConstructor = classMirror reflectConstructor constructor
  
    val fields : Seq[XSFieldInfo] = for (pp<-constructor.paramss;(p,i)<-pp.zipWithIndex) yield {
      new XSFieldInfo(p,i,ty,SerializableTypeInfo.this,iconSource)
    }
    
    val (fieldsAsBlocks,fieldsAsAttributes) = fields.partition{_.isBlock} 
  
    /** create the object! */
    def create(args:Seq[AnyRef]) : T = {
      //println("Creating "+name+" with "+args.length+" arguments")
      //for (a <- args) println("  "+(if (a==null) "" else a.getClass.toString)+" => "+a)
      reflectedConstructor.apply(args:_*).asInstanceOf[T] 
    }
  
    private[SerializableTypeInfo] val blockFieldsFromNames : Map[String,(XSFieldInfo,Option[SerializableTypeInfo[_ <: AnyRef]],Boolean)] = {
      var res : Map[String,(XSFieldInfo,Option[SerializableTypeInfo[_ <: AnyRef]],Boolean)] = Map.empty
      for (field<-fieldsAsBlocks) {
        def add(name:String,tpe:Option[SerializableTypeInfo[_ <: AnyRef]],isWrapper:Boolean) {
          if (res.contains(name)) error("Attribute "+name+" applies to more than one field")
          res+=name->(field,tpe,isWrapper)
        }
        if (field.isCollectionOrArray) add(field.nullElementName,null,false)
        if (field.wrapperName.isDefined) add(field.wrapperName.get,None,true)
        else field.xsinfo match {
          case Some(info) =>
            if (!field.obsoleteNames.isEmpty) {
              if (info.transitiveSubclasses.length!=1) error("Field "+field.originalName+" cannot have obsolete names as it is polymorphic.")
              else for (obsolete<-field.obsoleteNames) add(obsolete,Some(info.transitiveSubclasses.head),false)
            }
            field.overridingName match {
              case Some(name) =>
                if (info.transitiveSubclasses.length!=1) error("Field "+field.originalName+" cannot have an overridden name as it is polymorphic.")
                else add(name,Some(info.transitiveSubclasses.head),false)
              case None => 
                for (cl<-info.transitiveSubclasses) add(cl.name,Some(cl),false)
            }
          case None =>
            add(field.name,None,false)
            for (obsolete<-field.obsoleteNames) add(obsolete,None,false)
        }
      }
      res
    }
  
    private[SerializableTypeInfo] val attributeFieldsFromNames : Map[String,XSFieldInfo] = {
      var res : Map[String,XSFieldInfo] = Map.empty
      for (field<-fieldsAsAttributes) {
        def add(name:String) {
          if (res.contains(name)) error("Tag "+name+" applies to more than one field")
          res+=name->field
        }
        add(field.name)
        for (obsolete<-field.obsoleteNames) add(obsolete)
      }
      res
    }

    private[SerializableTypeInfo] val allFieldsFromNames : Map[String,XSFieldInfo] = {
      var res : Map[String,XSFieldInfo] = Map.empty
      for (field<-fields) {
        def add(name:String) {
          if (res.contains(name)) error("Tag "+name+" applies to more than one field")
          res+=name->field
        }
        add(field.name)
        for (obsolete<-field.obsoleteNames) add(obsolete)
      }
      res
    }

  }
    
  lazy val (dependencyInjectionInfo : DependencyInjectionInformation,fieldUpdatersMap: Map[XSFieldInfo,DependencyInjectionFunction]) = {
    val providers = new ListBuffer[DependencyInjectionFunction]
    val iconProviders = new ListBuffer[FunctionForField]
    val labelProviders = new ListBuffer[FunctionForField]
    val tooltipProviders = new ListBuffer[FunctionForField]
    val visibilityControllers = new ListBuffer[FunctionForField]
    val enabledControllers = new ListBuffer[FunctionForField]
    val errorChecks = new ListBuffer[FunctionForField]
    val extraText = new ListBuffer[ExtraDisplayFieldInfo]
    val customFields = new ListBuffer[CustomFieldInfo]
    val commands = new ListBuffer[CommandMethod]
    val classesToBlockForChildren:Seq[Class[_]] = clazz.getAnnotation(classOf[BlockDependencyInjection]) match {
      case a if a!=null && a.value!=null => a.value().toList
      case _ => Nil
    }
    val fieldUpdaters = new ListBuffer[FunctionForField]
    for (method<-ty.members.collect{case m if m.isMethod => m.asMethod }) if (!(method.isConstructor || method.isImplementationArtifact || method.isParamAccessor)) {
      val dp = hasAnnotation(method,typeDependencyProvider)
      val ip = getOptionalValue(method,typeIconProvider)
      val lp = getOptionalValue(method,typeLabelProvider)
      val ttp = getOptionalValue(method,typeTooltipProvider)
      val ptc = hasAnnotation(method,typePropagateToChildren)
      val ec = getOptionalValue(method,typeErrorCheck)
      val edf = hasAnnotation(method,typeExtraDisplayField)
      val cmd = hasAnnotation(method,typeXSCommand)
      val cdf = getOptionalValue(method,typeCustomEditable)
      val visC = getOptionalValue(method,typeVisibilityController)
      val enC = getOptionalValue(method,typeEnabledController)
      val fu = getOptionalValue(method,typeFieldUpdater)
      val numSpecial = (if (ip.isDefined) 1 else 0)+(if (lp.isDefined) 1 else 0)+(if (ttp.isDefined) 1 else 0)+(if (ec.isDefined) 1 else 0)+(if (edf) 1 else 0)+(if (cmd) 1 else 0)+(if (cdf.isDefined) 1 else 0)+(if (visC.isDefined) 1 else 0)+(if (enC.isDefined) 1 else 0)+(if (fu.isDefined) 1 else 0)
        if (numSpecial>1) error("Conflicting annotations on method "+method.name)
        if (dp||ptc||numSpecial>0) {
          if (visC.isDefined || enC.isDefined) { // check returns boolean
            if (!(method.returnType=:=typeBoolean)) error("method "+method.name.decoded+" should return a boolean")
          }
          def getFieldNamed(name:String) : XSFieldInfo = fields.find{_.name==name}.getOrElse(error("No field named "+name+" referenced in annotations on method "+method.name.decoded))
          val of : Option[Seq[XSFieldInfo]] = getOptionalArrayStrings(method,typeOnlyAffectedByFields).map{_.map{getFieldNamed(_)}}
          val fnparams = for (p<-method.paramss.flatten) yield rootMirror.runtimeClass(p.typeSignature.typeSymbol.asClass)
          val function = new DependencyInjectionFunction(fnparams,method,dp,ptc,of)
          def fff(name:Option[String]) : FunctionForField = new FunctionForField(function,if (name.get==null || name.get.isEmpty) None else name)
          if (ip.isDefined) iconProviders+=fff(ip)
          else if (lp.isDefined) labelProviders+=fff(lp)
          else if (ttp.isDefined) tooltipProviders+=fff(ttp)
          else if (ec.isDefined) errorChecks+=fff(ec)
          else if (visC.isDefined) visibilityControllers+=fff(visC)  
          else if (enC.isDefined) enabledControllers+=fff(enC) 
          else if (fu.isDefined) fieldUpdaters+=fff(fu) 
          else if (cdf.isDefined) customFields+=new CustomFieldInfo(function,method.name.decoded,new FieldDisplayOptions(method,iconSource),cdf.get) 
          else if (edf) extraText+=new ExtraDisplayFieldInfo(function,method.name.decoded,new FieldDisplayOptions(method,iconSource))
          else if (cmd) commands+=new CommandMethod(function,method.name.decoded,new FieldDisplayOptions(method,iconSource))
          else providers+=function
        }
    }
    val fieldNames : Set[String] = Set("delete")++fields.flatMap{_.namesOfExpectedFields}++extraText.toList.map{_.name}++customFields.toList.map{_.name} 
    val fieldAndSectionNames : Set[String] = fieldNames ++ fields.flatMap{_.displayOptions.editSection} ++ extraText.toList.flatMap{_.displayOptions.editSection}++ customFields.toList.flatMap{_.displayOptions.editSection}
    def check(list:List[FunctionForField],what:String,allowEmpty:Boolean,allowDuplicates:Boolean,allowSections:Boolean) {
      if (!allowDuplicates) for ((field,entries)<-list.groupBy{_.field}) if (entries.length>1) error("Conflicting "+what)
      if ((!allowEmpty) && list.exists{_.field.isEmpty}) error("Empty "+what)
      val okNames = if (allowSections) fieldAndSectionNames else fieldNames
      for (e<-list;name<-e.field) if (!okNames.contains(name)) error("Bad field name "+name+" in "+what)    
    }
    check(iconProviders.toList,"@IconProvider",true,false,true)
    check(labelProviders.toList,"@LabelProvider",true,false,true)
    check(tooltipProviders.toList,"@TooltipProvider",true,false,true)
    check(visibilityControllers.toList,"@VisibilityController",false,false,true)
    check(enabledControllers.toList,"@EnabledController",false,false,true)
    check(errorChecks.toList,"@ErrorCheck",true,true,true)
    check(fieldUpdaters.toList,"@FieldUpdater",false,false,false)
    val simpleErrorChecks : SimpleErrorChecks = {
      val buffer = new SimpleErrorChecksBuffer
      for (field<-fields) {
        //println("Getting simple error checks for field "+field.name)
        for (ann<-field.annotation(typeErrorIfBlank)) buffer.addErrorIfBlank(field,ann.getString("severity")) 
        for (ann<-field.annotation(typeErrorIfEmptyCollection)) buffer.addErrorIfEmptyCollection(field,ann.getString("severity")) 
        for (ann<-field.annotation(typeErrorIfNegative)) buffer.addErrorIfNegative(field,ann.getString("severity")) 
        for (ann<-field.annotation(typeErrorIfNotNumber)) buffer.addErrorIfNotNumber(field,ann.getBoolean("integer").getOrElse(false),ann.getDoubleOrNaN("min"),ann.getDoubleOrNaN("max"),ann.getString("severity")) 
        for (ann<-field.annotation(typeErrorIfNotRegex)) buffer.addErrorIfNotRegex(field,ann.getString("value").getOrElse(field.error("No arg to ErrorIfNotRegex annotation")),ann.getString("severity")) 
        for (ann<-field.annotation(typeErrorIfNotSuffix)) buffer.addErrorIfNotSuffix(field,ann.getString("value").getOrElse(field.error("No arg to ErrorIfNotSuffix annotation")),ann.getString("severity")) 
        for (info<-field.errorCheckInfo(typeErrorIfNotUniqueInObject)) buffer.addErrorIfNotUnique(field,info.key,UniquenessCheckResolution.getLocalBadness,info.severity)
        for (info<-field.errorCheckInfo(typeErrorIfNotUniqueInParent)) buffer.addErrorIfNotUnique(field,info.key,UniquenessCheckResolution.getParentBadness,info.severity)
        for (info<-field.errorCheckInfo(typeErrorIfNotGloballyUnique)) buffer.addErrorIfNotUnique(field,info.key,UniquenessCheckResolution.getGlobalBadness,info.severity)
      }
      buffer.get
    }
    val fieldUpdatersMap : Map[XSFieldInfo,DependencyInjectionFunction] = Map((for (u <- fieldUpdaters.toList;fname<-u.field;f<-fields.find{_.name==fname}) yield f->u.function):_*)
    //println("Class "+name+" field Updaters Map "+fieldUpdatersMap)
    val dependencyInjectionInfo = new DependencyInjectionInformation(providers.toList,iconProviders.toList,labelProviders.toList,tooltipProviders.toList,extraText.toList,customFields.toList,enabledControllers.toList,visibilityControllers.toList,errorChecks.toList,new CanPassToChildren(classesToBlockForChildren),simpleErrorChecks,commands.toList)
    (dependencyInjectionInfo,fieldUpdatersMap)
  }
  
  
  def deserializeError(cause:String) = throw new XSDeserializationError(clazz,cause)
  
  /** Get the field expected when an tag with a given name is found, plus (if it is serializable and not a wrapper) deserialization help, plus a boolean as to if this is just a wrapper. */
  def getBlockField(name:String) : (XSFieldInfo,Option[SerializableTypeInfo[_ <: AnyRef]],Boolean) = constructor.flatMap{_.blockFieldsFromNames.get(name)}.getOrElse(deserializeError("Unexpected attribute "+name))
  /** Get the field expected when an attribute with a given name is found */
  def getAttributeField(name:String) : XSFieldInfo = constructor.flatMap{_.attributeFieldsFromNames.get(name)}.
          orElse(blockFieldAsAttributeField(name)).getOrElse(deserializeError("Unexpected tag "+name))
  def getField(name:String) : XSFieldInfo = constructor.flatMap{_.allFieldsFromNames.get(name)}.getOrElse(deserializeError("Unexpected field "+name))
  // found an attribute that should really be a block, but can still read safely.
  def blockFieldAsAttributeField(name:String) : Option[XSFieldInfo] = constructor.flatMap{_.blockFieldsFromNames.get(name).map{_._1}}.filter{_.couldBeAttribute}
          
  lazy val fieldsAsBlocks = constructor.map{_.fieldsAsBlocks}.getOrElse(List.empty)
  lazy val fieldsAsAttributes = constructor.map{_.fieldsAsAttributes}.getOrElse(List.empty)
  lazy val fields = constructor.map{_.fields}.getOrElse(List.empty)
  lazy val numFields = fields.length
  def create(args:Seq[AnyRef]) : T = constructor match {
    case Some(c) => c.create(args)
    case None => error("No constructor defined")
  }
  
  def newElement() : T =  constructor match {
    case Some(c) =>
      val args = for (f<-c.fields) yield f.newPossiblyMultipleElement()
      c.create(args)
    case None => error("No constructor defined")
  }
  
  lazy val treeNodeFields = fieldsAsBlocks.filter{_.isIndividuallyEditable}
  //lazy val tableNodeFields = fieldsAsBlocks.filter{_.isTableEditable}
  lazy val tableAndInlineNodeFields = fieldsAsBlocks.filter{n=>n.isTableEditable || n.isInlineEditable}
  
  lazy val uniquenessCheckLocal : FieldsToCheckForUniqueness = getUniquenessCheck(typeErrorIfNotUniqueInObject)
  lazy val uniquenessCheckParent : FieldsToCheckForUniqueness = getUniquenessCheck(typeErrorIfNotUniqueInParent)
  lazy val uniquenessCheckGlobal : FieldsToCheckForUniqueness = getUniquenessCheck(typeErrorIfNotGloballyUnique)

  private def getUniquenessCheck(annotationClass:reflect.runtime.universe.Type) : FieldsToCheckForUniqueness = {
    val res = new collection.mutable.HashMap[UniquenessClass,ListBuffer[XSFieldInfo]]
    for (field<-fields; eci<-field.errorCheckInfo(annotationClass)) res.getOrElseUpdate(eci.key,new ListBuffer[XSFieldInfo]) += field
    if (res.isEmpty) FieldsToCheckForUniqueness.empty else {
      val list = for ((key,l)<-res) yield new FieldsToCheckForUniquessForGivenKey(key,l.toList)      
      new FieldsToCheckForUniqueness(list.toList)
    } 
  }
  //
  // Functions that apply to modifying an object (eg by XS editing)
  //
  
  
  /**
   * Modify the original object by deleting some values of the field field.
   * The values to be deleted are the 0 based indices given in the toDelete argument.
   */
  def deleteField(original:T,field:XSFieldInfo,toDelete:Set[Int]) : T = {
    def delete(seq:IndexedSeq[AnyRef]) = for ((c,i)<-seq.zipWithIndex if !toDelete.contains(i)) yield c
    alterSingleField(original,field,_ => null,delete _)
  }
  def deleteFieldAnyRef(original:AnyRef,field:XSFieldInfo,toDelete:Set[Int]) : AnyRef = deleteField(original.asInstanceOf[T],field,toDelete).asInstanceOf[AnyRef]
  
  /**
   * Add the given field "element" to the "field" field of the "orginal" object, either at the end of the seq or at position desiredIndex.
   */
  def addField(original:T,desiredIndex:Option[Int],field:XSFieldInfo,element:AnyRef):T = {
    def insert(seq:IndexedSeq[AnyRef]) = desiredIndex match {
      case Some(n) => seq.patch(n,List(element),0)
      case None => seq:+element
    }
    alterSingleField(original,field,_ => element,insert _)
  }
  def addFieldAnyRef(original:AnyRef,desiredIndex:Option[Int],field:XSFieldInfo,element:AnyRef):AnyRef =addField(original.asInstanceOf[T],desiredIndex,field,element).asInstanceOf[AnyRef]
  
  /**
   * Change the given field "field" to the new value "element" of the "orginal" object, at the position changedIndex.
   */
  def changeField(original:T,changedIndex:Int,field:XSFieldInfo,element:AnyRef) : T = {
    alterSingleField(original,field,_ => element,_.updated(changedIndex,element))
  }
  
  /**
   * Change the given field "field" to the new value "element" of the "orginal" object. 
   * This is different from changeField in that if the field is a collection, then the argument to this is the whole collection, whereas changeField will only change one field in a collection.
   */
  def setField(original:T,field:XSFieldInfo,element:AnyRef) : T = { 
    fieldUpdatersMap.get(field) match {
      case Some(fu) =>
        //println("Updating via field updater rather than constructor")
        fu.apply(scala.reflect.runtime.currentMirror.reflect(original.asInstanceOf[AnyRef]),List(element)).asInstanceOf[T]
      case None =>
        constructor match {
          case Some(c) =>
            val newfieldvalues = for (f<-c.fields) yield {
              val current = f.getField(original).asInstanceOf[AnyRef]
              if (f eq field) element else current
            }
            c.create(newfieldvalues)
          case None => throw new IllegalArgumentException
        }
    }
  }
  def setFieldAnyRef(original:AnyRef,field:XSFieldInfo,element:AnyRef) : AnyRef = setField(original.asInstanceOf[T],field,element).asInstanceOf[AnyRef]
  
  
  def changeFieldAnyRef(original:AnyRef,changedIndex:Int,field:XSFieldInfo,element:AnyRef) : AnyRef = changeField(original.asInstanceOf[T],changedIndex,field,element).asInstanceOf[AnyRef] 
  
  /**
   * Alter (by making a changed copy) the "original" object of this type by affecting a single field "field". If said field
   * is a collection, then the function "collection" will make a new collection out of the old one. If it is not a collection,
   * then the nonColleciton function will make a new element out of the old one.
   */
  private def alterSingleField(original:T,field:XSFieldInfo,nonCollection: AnyRef=>AnyRef,collection:IndexedSeq[AnyRef]=>IndexedSeq[AnyRef]) : T = {
    constructor match {
      case Some(c) =>
        val newfieldvalues = for (f<-c.fields) yield {
          val current = f.getField(original).asInstanceOf[AnyRef]
          if (f eq field) {
            if (field.isCollectionOrArray) {
              val currentCollection  : IndexedSeq[AnyRef] = f.getAllFieldElements(original).map{_.asInstanceOf[AnyRef]}(scala.collection.breakOut)
              f.collectionOfBuffer(collection(currentCollection))
            } else nonCollection(current)
          } else current
        }
        c.create(newfieldvalues)
      case None => throw new IllegalArgumentException
    }
    
  }
  
  val classTag : ClassTag[T] = ClassTag(clazz)
  def deserializeInto(reader:XMLStreamReader,original:AnyRef,loadBefore:Option[(XSFieldInfo,Int)]) : (T,Set[EqualityByPointerEquality[AnyRef]]) =
    XMLDeserialize.deserializeInto[T](reader,this,original.asInstanceOf[T],loadBefore)
    
    
  //
  // functions that pertain to resource bundles
  //
    
  private val cacheTextResources = new scala.collection.mutable.HashMap[Locale,TextLocalizationResources]
  def textResources(locale:Locale) : TextLocalizationResources = cacheTextResources.getOrElseUpdate(locale,TextLocalizationResources.getUncached(locale, clazz))
  
  //
  // code that pertains to EditPanes
  //
  private[this] val panesNoDelete = new collection.mutable.HashMap[Locale,DetailsPaneFields] // cache
  private[this] val panesDelete = new collection.mutable.HashMap[Locale,DetailsPaneFields]  // cache
  
  def getPane(locale:Locale,mayDelete:Boolean) : DetailsPaneFields = (if (mayDelete) panesDelete else panesNoDelete).getOrElseUpdate(locale,DetailsPaneFields(this,locale,mayDelete))
  
  //
  // code pertaining to templates
  //
  lazy val htmlTemplate : Option[xml.Node] = {
    try {
      val is = clazz.getResourceAsStream(classSymbol.name.decoded+".template.xml")
      val seq = XML.load(is)
      Some(seq)
    } catch { case _ : Exception => None }
  }
}



class NotSerializableException extends Exception
class XSSpecificationError(clazz : java.lang.Class[_],cause:String) extends Exception("Class "+clazz+" : "+cause)
class XSDeserializationError(clazz : java.lang.Class[_],cause:String) extends Exception("Class "+clazz+" : "+cause)

object SerializableTypeInfo {
  private val cache = new scala.collection.mutable.HashMap[java.lang.Class[_],Option[SerializableTypeInfo[_]]]
  private[impl] def getSymbol(clazz:Class[_]) = universe.runtimeMirror(clazz.getClassLoader).classSymbol(clazz)
  private[impl] val symbolGenTraversable = universe.typeOf[GenTraversable[_]].typeSymbol
  private[impl] val symbolOption = universe.typeOf[Option[_]].typeSymbol
  private[impl] val symbolList = universe.typeOf[List[_]].typeSymbol
  private[impl] val symbolSeq = universe.typeOf[Seq[_]].typeSymbol
  private[impl] val symbolIndexedSeq = universe.typeOf[IndexedSeq[_]].typeSymbol
  private[impl] val symbolVector = universe.typeOf[Vector[_]].typeSymbol
  private[impl] val symbolSet = universe.typeOf[Set[_]].typeSymbol
  private[impl] val symbolTraversable = universe.typeOf[Traversable[_]].typeSymbol
  private[impl] val symbolArray = universe.typeOf[Array[_]].typeSymbol

  private[impl] val typeXSName = universe.typeOf[XSName]
  private[impl] val typeXSIcon = universe.typeOf[XSIcon]
  private[impl] val typeXSWrapper = universe.typeOf[XSWrapper]
  private[impl] val typeXSConstructor = universe.typeOf[XSConstructor]
  private[impl] val typeXSObsoleteName = universe.typeOf[XSObsoleteName]
  private[impl] val typeXSSerializable = universe.typeOf[XS]
  private[impl] val typeXSSerializeAsBlock = universe.typeOf[XSSerializeAsBlock]
  private[impl] val typeXSSerializeAsAttribute = universe.typeOf[XSSerializeAsAttribute]
  private[impl] val typeDefaultValue = universe.typeOf[DefaultValue]
  private[impl] val typeStringEditable = universe.typeOf[StringEditable]
  private[impl] val typeBooleanEditable = universe.typeOf[BooleanEditable]
  private[impl] val typeIndividuallyEditable = universe.typeOf[IndividuallyEditable]
  private[impl] val typeTableEditable = universe.typeOf[TableEditable]
  private[impl] val typeInlineEditable = universe.typeOf[InlineEditable]
  private[impl] val typeCustomEditable = universe.typeOf[CustomEditable]
  private[impl] val typeCustomPopup = universe.typeOf[CustomPopup]
  private[impl] val typeEditSection = universe.typeOf[EditSection]
  private[impl] val typeOrderingPriority = universe.typeOf[OrderingPriority]
  private[impl] val typeMaxLength = universe.typeOf[MaxLength]
  private[impl] val typeWidthHint = universe.typeOf[WidthHint]
  private[impl] val typeDisplayOnly = universe.typeOf[DisplayOnly]
  private[impl] val typeMultiline = universe.typeOf[Multiline]
  private[impl] val typeHideName = universe.typeOf[HideName]
  private[impl] val typeWholeLine = universe.typeOf[WholeLine]
  private[impl] val typeMaxChildren = universe.typeOf[MaxChildren]
  private[impl] val typeRequiredOptions = universe.typeOf[RequiredOptions]
  private[impl] val typeSuggestedOptions = universe.typeOf[SuggestedOptions]
  private[impl] val typeDoNotTrim = universe.typeOf[DoNotTrim]
  private[impl] val typeImageEditable = universe.typeOf[ImageEditable]
  private[impl] val typeResolveNetworkReferences = universe.typeOf[ResolveNetworkReferences]
  private[impl] val typeNoBorder = universe.typeOf[NoBorder]

  private[impl] val typeDependencyProvider = universe.typeOf[DependencyProvider]
  private[impl] val typeIconProvider = universe.typeOf[IconProvider]
  private[impl] val typeLabelProvider = universe.typeOf[LabelProvider]
  private[impl] val typeTooltipProvider = universe.typeOf[TooltipProvider]
  private[impl] val typePropagateToChildren = universe.typeOf[PropagateToChildren]
  private[impl] val typeErrorCheck = universe.typeOf[ErrorCheck]
  private[impl] val typeExtraDisplayField = universe.typeOf[ExtraDisplayField]
  private[impl] val typeXSCommand = universe.typeOf[XSCommand]
  private[impl] val typeVisibilityController = universe.typeOf[VisibilityController]
  private[impl] val typeEnabledController = universe.typeOf[EnabledController]
  private[impl] val typeOnlyAffectedByFields = universe.typeOf[OnlyAffectedByFields]
  private[impl] val typeFieldUpdater = universe.typeOf[FieldUpdater]
  

  private[impl] val typeExpandOnFirstDisplay = universe.typeOf[ExpandOnFirstDisplay]

  private[impl] val typeErrorIfBlank = universe.typeOf[ErrorIfBlank]
  private[impl] val typeErrorIfEmptyCollection = universe.typeOf[ErrorIfEmptyCollection]
  private[impl] val typeErrorIfNotNumber = universe.typeOf[ErrorIfNotNumber]
  private[impl] val typeErrorIfNegative = universe.typeOf[ErrorIfNegative]
  private[impl] val typeErrorIfNotRegex = universe.typeOf[ErrorIfNotRegex]
  private[impl] val typeErrorIfNotSuffix = universe.typeOf[ErrorIfNotSuffix]
  private[impl] val typeErrorIfNotUniqueInObject = universe.typeOf[ErrorIfNotUniqueInObject]
  private[impl] val typeErrorIfNotUniqueInParent = universe.typeOf[ErrorIfNotUniqueInParent]
  private[impl] val typeErrorIfNotGloballyUnique = universe.typeOf[ErrorIfNotGloballyUnique]

  private[impl] val typeBoolean = universe.typeOf[Boolean]
  
  private[impl] val classOnObsoleteCallback = classOf[OnObsoleteCallback]

  private val syncronizeValidityChecks = new Object // object used for making sure validity checks are synchronized. 

  def get[T](clazz:Class[T]) : Option[SerializableTypeInfo[_]] = {
    val res = getWithoutCheckingValidity(clazz)
    for (r<-res) r.checkValid()
    res
  }
  
  def getWithoutCheckingValidity[T](clazz:Class[T]) : Option[SerializableTypeInfo[_]] = cache.getOrElseUpdate(clazz,{try { Some(new SerializableTypeInfo(clazz.asInstanceOf[Class[_ <: AnyRef]]))} catch { case _ : NotSerializableException => None }})

    /** If there is an annotation of the specified type, get the value of the field called "value" which is a string. Return Some(null) if the annotation exists, but does not have the stated field. */
  def annotationField(symbol:reflect.runtime.universe.Symbol,annotation:reflect.runtime.universe.Type,fieldName:String) : Option[Any] = {
    for (ann<-symbol.annotations.find{_.tpe =:= annotation}) yield {
      val nonnull = for (v<-ann.javaArgs.find{_._1.decoded==fieldName};universe.Constant(s)=v._2.asInstanceOf[universe.LiteralArgument].value) yield s
      nonnull.getOrElse(null)
    }
  }


  
  /** If there is an annotation, get the value of the field called "value" which is a string. Return null if the annotation exists, but does not have a value field. */
  def getOptionalValue(symbol:reflect.runtime.universe.Symbol,annotation:reflect.runtime.universe.Type) : Option[String] = annotationField(symbol,annotation,"value").collect{case null => null; case s:String => s}

    /** If there is an annotation, get the value of the field called "value" which is an int. */
  def getOptionalIntValue(symbol:reflect.runtime.universe.Symbol,annotation:reflect.runtime.universe.Type) : Option[Int] = {
    for (
        ann<-symbol.annotations.find{_.tpe =:= annotation};
        v<-ann.javaArgs.find{_._1.decoded=="value"};
        universe.Constant(s:Int)=v._2.asInstanceOf[universe.LiteralArgument].value) yield s
  }


  /** If there is an annotation with a field called "value" which is a string, get that value as a string */
  def getOptionalArrayStrings(symbol:reflect.runtime.universe.Symbol,annotation:reflect.runtime.universe.Type) : Option[Seq[String]] = 
    for (ann<-symbol.annotations.find{_.tpe =:= annotation};v<-ann.javaArgs.find{_._1.decoded=="value"}) yield
      for (vv<-v._2.asInstanceOf[universe.ArrayArgument].args.toSeq;
           universe.Constant(s:String)=vv.asInstanceOf[universe.LiteralArgument].value
           ) yield s
    
        
      /** If there is an annotation on a symbol */
  def hasAnnotation(symbol:reflect.runtime.universe.Symbol,annotation:reflect.runtime.universe.Type) : Boolean = 
    symbol.annotations.exists{_.tpe =:= annotation}


  def annotation(symbol:reflect.runtime.universe.Symbol,annotation:reflect.runtime.universe.Type) : Option[AnnotationInfo] = symbol.annotations.find{_.tpe =:= annotation}.map{new AnnotationInfo(_)}
}

class AnnotationInfo(ann:reflect.runtime.universe.Annotation) {
  //println("Found annotation for "+ann.tpe.typeSymbol.name.decoded)
  def getField(fieldName:String) = ann.javaArgs.find{_._1.decoded==fieldName}
  def getConstantField(fieldName:String) : Option[Any] = for (v<-getField(fieldName);universe.Constant(s)=v._2.asInstanceOf[universe.LiteralArgument].value) yield s
  def getArrayField(fieldName:String) : Option[Seq[Any]] = for (v<-getField(fieldName)) yield {
    for (vv<-v._2.asInstanceOf[universe.ArrayArgument].args.toSeq;universe.Constant(s)=vv.asInstanceOf[universe.LiteralArgument].value) yield s
  }
  def getString(fieldName:String) : Option[String] = getConstantField(fieldName).collect{case s:String=>s}
  def getInt(fieldName:String) : Option[Int] = getConstantField(fieldName).collect{case s:Int=>s}
  def getDouble(fieldName:String) : Option[Double] = getConstantField(fieldName).collect{case s:Double=>s}
  def getDoubleOrNaN(fieldName:String) : Double = getDouble(fieldName).getOrElse(Double.NaN)
  def getBoolean(fieldName:String) : Option[Boolean] = getConstantField(fieldName).collect{case s:Boolean=>s}
}