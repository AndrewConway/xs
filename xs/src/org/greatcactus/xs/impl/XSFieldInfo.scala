/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */

package org.greatcactus.xs.impl
import SerializableTypeInfo._
import scala.reflect.runtime._
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer
import scala.collection.GenTraversable
import org.greatcactus.xs.api.edit.SuggestedOptions
import org.greatcactus.xs.api.edit.RequiredOptions
import org.greatcactus.xs.api.icon.IconManifests
import org.greatcactus.xs.api.icon.Icon
import java.util.Locale
import scala.util.Try
import org.greatcactus.xs.api.display.TextLocalizationSource
import org.greatcactus.xs.api.display.TextLocalizationResources
import org.greatcactus.xs.api.display.PrefixedTextLocalizationResources
import org.greatcactus.xs.api.edit.FiniteOptionList
import org.greatcactus.xs.api.edit.FiniteOptionListWithPrettyNames
import org.greatcactus.xs.api.edit.FiniteOptionListWithIcons

/**
 * Information on a field inside a class. This is not designed to be used outside of XS
 */
class XSFieldInfo(val fieldSymbol:reflect.runtime.universe.Symbol,val index:Int,parentType:universe.Type,parentInfo:SerializableTypeInfo[_],iconSource:IconManifests) extends TextLocalizationSource with GeneralizedField {
    def parentClass = parentInfo.clazz
    val originalName = fieldSymbol.name.decoded
    
    val overridingName = getOptionalValue(fieldSymbol,typeXSName).map{n=>if (n==null || n.isEmpty) originalName else n}
    val wrapperName = getOptionalValue(fieldSymbol,typeXSWrapper).map{n=>if (n==null || n.isEmpty) originalName else n}
    
    val name = overridingName.getOrElse(fieldSymbol.name.decoded)
    val nullElementName = "null-"+name
    
    val ftype = fieldSymbol.typeSignature
    private def ptypeIs(tpe:universe.Symbol) = ftype.baseType(tpe) != universe.NoType
    
    val isScalaOption = ptypeIs(symbolOption)
    val isScalaCollection = ptypeIs(symbolGenTraversable)
    def isParticularScalaCollection(collectionType:universe.Symbol) = isScalaCollection && ptypeIs(collectionType) // this is not a good solution as it has false positives.
    val isScalaList = isParticularScalaCollection(symbolList)
    val isScalaSeq = isParticularScalaCollection(symbolSeq)
    val isScalaIndexedSeq = isParticularScalaCollection(symbolIndexedSeq)
    val isScalaVector = isParticularScalaCollection(symbolVector)
    val isScalaSet = isParticularScalaCollection(symbolSet)
    val isScalaTraversable = isParticularScalaCollection(symbolTraversable)

    val isArray = ftype.baseType(symbolArray) != universe.NoType

    val isCollectionOrArray = isArray || isScalaCollection || isScalaOption
    val isCollectionOrArrayButNotOption = isArray || isScalaCollection 
  
    val baseType = if (isCollectionOrArray) {
      val superclass = if (isScalaCollection) symbolGenTraversable else if (isScalaOption) symbolOption else if (isArray) symbolArray else throw new IllegalArgumentException(fieldSymbol.toString)
      val refinedType = ftype.baseType(superclass)
       //println("Refined type "+refinedType)
       //val universe.ClassDef() = argumentType
       val universe.TypeRef(_, _, argumentType :: Nil) = refinedType
       //println("Arg type "+argumentType)
       argumentType
    } else ftype
    

    /** The maximum number of children this field can have. -1 means infinite. */
    val maxChildren = if (isScalaOption) 1 else if (isCollectionOrArray) annotationValueInt(typeMaxChildren,-1).getOrElse(-1) else 1
    
    val baseClassName = baseType.typeSymbol.fullName
    val baseClass : Class[_] = baseClassName match {
      case "scala.Byte" => java.lang.Integer.TYPE
      case "scala.Short" => java.lang.Short.TYPE
      case "scala.Char" => java.lang.Character.TYPE
      case "scala.Int" => java.lang.Integer.TYPE
      case "scala.Long" => java.lang.Long.TYPE
      case "scala.Float" => java.lang.Float.TYPE
      case "scala.Double" => java.lang.Double.TYPE
      case "scala.Boolean" => java.lang.Boolean.TYPE
      case _ => parentClass.getClassLoader.loadClass(baseClassName)
    }
    val baseClassIsPrimitive = baseClass.isPrimitive()
    
   
    val fixedOptions : Option[EnumeratedOptions] = {
      val sco = baseClass.getAnnotation(classOf[SuggestedOptions])
      val rco = baseClass.getAnnotation(classOf[RequiredOptions])
      val rfo = annotationValueStringArray(typeRequiredOptions)
      val sfo = annotationValueStringArray(typeSuggestedOptions)
      if (rfo.isDefined) Some(new EnumeratedOptions(true,rfo.get.toList,Map.empty,Map.empty,None))
      else if (sfo.isDefined) Some(new EnumeratedOptions(true,sfo.get.toList,Map.empty,Map.empty,None))
      else if (rco!=null) Some(new EnumeratedOptions(true,rco.value.toList,Map.empty,Map.empty,Some(baseClass)))
      else if (sco!=null) Some(new EnumeratedOptions(false,sco.value.toList,Map.empty,Map.empty,Some(baseClass)))
      else if (baseClass.getSuperclass() == classOf[java.lang.Enum[_]]) Some(new EnumeratedOptions(true,baseClass.getEnumConstants().map{_.toString}.toList,Map.empty,Map.empty,Some(baseClass)))  
      else if (classOf[FiniteOptionList].isAssignableFrom(baseClass)) Some({
                // see if there is a Scala style companion object with an allValues method
        ValueOfString.searchForCompanionObjectMethod(baseClass,"allValues",Nil) match {
          case Some(m) => 
            val possibilities : List[Any] = m.apply() match {
              case s:Seq[Any] => s.toList 
              case s:Array[Any] => s.toList
              case _ => error("Unacceptable result for allValues method for "+baseClass.getName())
            }
            var prettyPrint : Map[String,String] = Map.empty
            var icons : Map[String,String] = Map.empty
            for (p<-possibilities) {
              p match {
                case x:FiniteOptionListWithPrettyNames if x.humanReadableName!=null => prettyPrint+=x.toString->x.humanReadableName
                case _ =>
              }
              p match {
                case x:FiniteOptionListWithIcons if x.icon!=null => icons+=x.toString->x.icon
                case _ =>
              }
            }
            new EnumeratedOptions(true,possibilities.map{_.toString},prettyPrint,icons,Some(baseClass))
          case None =>
            error("No companion object with allValues method for "+baseClass.getName())
        }
      })
      else None // should handle Scala enumerations as well
    } 
      
    
    val baseClassTag : ClassTag[_] = ClassTag(baseClass) 

    val xsinfo : Option[SerializableTypeInfo[_]]= getWithoutCheckingValidity(baseClass) // plain get can cause an infinite loop. See comment on constructor field of SerializableTypeInfo.
    val isAssertedAsBlock = hasAnnotation(typeXSSerializeAsBlock)
    val isAssertedAsAttribute = hasAnnotation(typeXSSerializeAsAttribute)
    def couldBeAttribute = !xsinfo.isDefined
    val isBlock = xsinfo.isDefined || isAssertedAsBlock || wrapperName.isDefined || (baseClassName=="java.lang.String" && !isAssertedAsAttribute)
    
    val getMethod = {
      val m = parentType.member(universe.newTermName(originalName))
      if (m == universe.NoSymbol) error("No method "+originalName)
      else if (m.isMethod) {
        val mm = m.asMethod
        if (mm.isOverloaded) error("Method "+originalName+" is overloaded")
        else if (!mm.paramss.isEmpty) error("Method "+originalName+" takes parameters")
        else if (!(mm.returnType =:= ftype )) error("Method "+originalName+" has the wrong return type")
        else mm
      } else error("Problem with accessing method "+originalName+" - possibly you need to make it a val in the constructor definition or remove other methods with the same name.")
    }
    
    /** Get the value of this field for a given parent instance */
    def getField(parentInstance:AnyRef) = {
     // println("Trying to get field "+name+" of "+parentInstance)
      val instanceMirror = rootMirror.reflect(parentInstance)
      val methodMirror = instanceMirror.reflectMethod(getMethod)
      methodMirror.apply()
    }
    
    /** Get the value of this field, converted to a string. May be null. */
    def getFieldAsString(parentInstance:AnyRef) : String = {
      getField(parentInstance) match {
        case null => null 
        case a:Array[_] => if (a.length==0) null else CollectionStringUtil.joinSemicolonListEscaped(a)
        case a:GenTraversable[_] => if (a.isEmpty) null else CollectionStringUtil.joinSemicolonListEscaped(a)
        case a:Option[_] => if (a.isEmpty) null else a.get.toString
        case field => field.toString
      }

    }
    
    def getFieldAsStringCollectionLengthInfo(parentInstance:AnyRef,humanEdited:Array[Option[TrimInfo]]) : Option[LengthInfo] = { 
      if (isCollectionOrArrayButNotOption) {
        val fields = getAllFieldElements(parentInstance);
        if (fields==null) None
        else Some(new CollectionLengthInfo(fields.toArray.map{e=>if (e==null) "" else e.toString},humanEdited)) 
      } else {
        val field = getFieldAsString(parentInstance)
        if (field==null) None
        else Some(new SingleLengthInfo(field,if (humanEdited.length>0) humanEdited(0) else None))
      }
    }
    
    /** Like getField, except get all the fields as a collection  */
    def getAllFieldElements(parentInstance:AnyRef) : GenTraversable[Any] = {
      getField(parentInstance) match {
        case null => Nil
        case a:Array[_] => a
        case a:GenTraversable[_] => a
        case a:Option[_] => a.toList
        case atom => List(atom)
      }
    }
    
    /** True iff there are no elements for this field. Either it is null, or an empty collection. */
    def isEmpty(parentInstance:AnyRef) : Boolean = getAllFieldElements(parentInstance:AnyRef).isEmpty
    
    val obsoleteNames : Seq[String] = getOptionalArrayStrings(fieldSymbol,typeXSObsoleteName).toSeq.flatten

    val parser = { 
      if (xsinfo.isDefined) null // not needed
      else try {
       ValueOfString.deserializer(baseClass) 
      } catch {case _:IllegalArgumentException => error("Could not find deserializer for "+name) }
    }
    // val defaultStringRep = getOptionalValue(fieldSymbol,typeDefaultValue) // the default value for a new instance.
    val defaultActivelySetValue : Option[AnyRef] = for (s<-getOptionalValue(fieldSymbol,typeDefaultValue)) yield parser(s)
    

    def newSingleElement() : AnyRef = xsinfo match {
      case Some(xsi) => xsi.newElement().asInstanceOf[AnyRef]
      case None => defaultElementValue
    }
    
    val defaultElementValue : AnyRef = defaultActivelySetValue.getOrElse{baseClassName match {
          case "scala.Byte" => new java.lang.Byte(0.toByte) 
          case "scala.Short" => new java.lang.Short(0.toShort)
          case "scala.Char" => new java.lang.Character(0)
          case "scala.Int" => new java.lang.Integer(0)
          case "scala.Long" => new java.lang.Long(0L)
          case "scala.Float" => new java.lang.Float(0.0f)
          case "scala.Double" => new java.lang.Double(0.0)
          case "scala.Boolean" => java.lang.Boolean.FALSE
          case _ => null
        }
    }
    /** What value should be used if nothing is given during deserialization. This is null, except for primitives which NEED something. */
    val leftOutValue : Option[AnyRef] = if (isCollectionOrArray || !baseClassIsPrimitive) None else Some{defaultElementValue}
      
    
    def newPossiblyMultipleElement() : AnyRef = {
      if (isCollectionOrArray) collectionOfBuffer(IndexedSeq())
      else newSingleElement()
    }
            
    /** True if the string representation should be trimmed before parsing */
    val shouldTrimStringrep = !hasAnnotation(typeDoNotTrim)
    
    /** value for a blank string. If the parser interprets a blank string, then it. Otherwise null. */
    val nullValue = try { parser("")} catch { case _:Exception => null}
    
    /** Given a string representation of the baseClass object, get the object. This Does not work for semicolon separated lists. It will return None if there is an error in the parsing. */
    def parseStringSingleSafe(stringrep:String) : Try[AnyRef] = {
      val s = if (stringrep==null) "" else if (shouldTrimStringrep) stringrep.trim else stringrep
      Try { parser(s) }
    }
    /** Given a string representation of the baseClass object, get the object. This Does not work for semicolon separated lists.  */
    def parseStringSingle(stringrep:String) : AnyRef = {
      val s = if (stringrep==null) "" else if (shouldTrimStringrep) stringrep.trim else stringrep
      parser(s) 
    }
    /** Given a string representation of the object, get the object. This also works for semicolon separated lists. Return Some(result) or None if the string is illegal. */
    def parseStringPossiblyMultipleSafe(stringrep:String) : Try[AnyRef] = {
      val s = if (stringrep==null) "" else if (shouldTrimStringrep) stringrep.trim else stringrep
      Try {
          if (isScalaOption) {
            if (s.isEmpty) None    
            else Some(parser(s))
          } else if (isCollectionOrArray) {
            val buffer = new ArrayBuffer[AnyRef]
            if (!stringrep.isEmpty()) {
              val textreps : Array[String] =CollectionStringUtil.separateSemicolonListEscaped(stringrep) 
              for (s<-textreps) {
                val ts = if (shouldTrimStringrep) s.trim else s
                buffer+= (if (ts.isEmpty) nullValue else parser(ts))
              }
            }
            collectionOfBuffer(buffer)
          } else if (s.isEmpty && !baseClassIsPrimitive) null else parser(s)
      } 
    }
    //def getBuffer : UnsafeBuffer[_] = parser.getBuffer
    def collectionOfBuffer(buffer:IndexedSeq[AnyRef]) : AnyRef = {
      val n = buffer.length
      if (isArray) {
        //println("Base class tag "+baseClassTag.runtimeClass)
        val array = baseClassTag.newArray(n)
        //println("Array type is "+array.getClass)
        for (i<-0 until n) {
          //val elem = buffer(i)
          //println(" elem "+elem+"   "+elem.getClass)
          java.lang.reflect.Array.set(array,i,buffer(i))
        }
        array
      } else {
        if (isScalaOption) buffer.length match {
          case 0 => None
          case 1 => Some(buffer(0))
          case n => throw new IllegalArgumentException("Found "+n+" elements for option")
        } else if (isScalaList) buffer.toList
        else if (isScalaVector) buffer.toVector
        else if (isScalaSet) buffer.toSet
        else if (isScalaIndexedSeq) buffer.toIndexedSeq
        else if (isScalaSeq) buffer.toSeq
        else if (isScalaTraversable) buffer.toSeq
        else error("Unimplemented collection type")
      }
    }
    lazy val emptyCollection = collectionOfBuffer(IndexedSeq())
    
    def error(cause:String) = throw new XSSpecificationError(parentClass,cause+", field "+originalName)
    
    lazy val blockFieldLookup : Map[String,SerializableTypeInfo[_ <: AnyRef]] = Map.empty ++ (for (c<-xsinfo.get.transitiveSubclasses) yield c.name->c)
        
    def getBlockField(name:String) : Option[SerializableTypeInfo[_ <: AnyRef]] = if (xsinfo.isDefined) Some(blockFieldLookup.getOrElse(name,error("Can't interpret "+name+" for field "+this.name))) else None
      
    def annotation(annotationClass:reflect.runtime.universe.Type) : Option[AnnotationInfo] = SerializableTypeInfo.annotation(fieldSymbol,annotationClass)
    def annotation(annotationClass:reflect.runtime.universe.Type,fieldName:String) : Option[Any] = SerializableTypeInfo.annotationField(fieldSymbol, annotationClass, fieldName)
    def hasAnnotation(annotationClass:reflect.runtime.universe.Type) = SerializableTypeInfo.hasAnnotation(fieldSymbol, annotationClass)
    def annotationString(annotationClass:reflect.runtime.universe.Type,fieldName:String) : Option[String] = annotation(annotationClass,fieldName).map{_.asInstanceOf[String]} 
    def annotationInt(annotationClass:reflect.runtime.universe.Type,fieldName:String,defaultValue:Int) : Option[Int] = annotation(annotationClass,fieldName).map{v=>if (v==null) defaultValue else v.asInstanceOf[Int]} 
    def annotationValueString(annotationClass:reflect.runtime.universe.Type) : Option[String] = annotationString(annotationClass,"value")
    def annotationValueStringArray(annotationClass:reflect.runtime.universe.Type) : Option[Seq[String]] = SerializableTypeInfo.getOptionalArrayStrings(fieldSymbol,annotationClass)
    def annotationValueInt(annotationClass:reflect.runtime.universe.Type,defaultValue:Int) : Option[Int] = annotationInt(annotationClass,"value",defaultValue) 
    //
    // Information on editing.
    //
    
    val isIndividuallyEditable : Boolean = hasAnnotation(typeIndividuallyEditable)
    if (isIndividuallyEditable && !xsinfo.isDefined) error("Individially Editable, but not xs serializable")
    val isExpandOnFirstDisplay : Boolean = hasAnnotation(typeExpandOnFirstDisplay)
    val isStringEditable : Boolean = hasAnnotation(typeStringEditable)
    val isBooleanEditable : Boolean = hasAnnotation(typeBooleanEditable)
    val isImageEditable : Boolean = hasAnnotation(typeImageEditable)
    val isTableEditable : Boolean = hasAnnotation(typeTableEditable)
    val isInlineEditable : Boolean = hasAnnotation(typeInlineEditable)
    def isTableOrInlineEditable : Boolean = isTableEditable || isInlineEditable
    val customPopup : Option[String] = annotationValueString(typeCustomPopup)
    
    val resolveNetworkReferences : Boolean = hasAnnotation(typeResolveNetworkReferences)
    val mayNotAddChildren = hasAnnotation(typeMayNotAddChildren)

    if (isBooleanEditable && (isCollectionOrArray || baseClass!=java.lang.Boolean.TYPE)) error("Boolean editable field should be boolean")
    if (isIndividuallyEditable && !xsinfo.isDefined) error("Individually editable field should be of an @XSE class")
    if (isTableEditable) xsinfo match {
      case None => error("Table editable field should be of an @XS class")
      case Some(xsi) if !xsi.subclasses.isEmpty => error("Table editable field should be of an @XS class without subclasses.")
      case _ =>
    }
    if (isInlineEditable && xsinfo.isEmpty) error("InlineEditable field should be of an @XS class")  
    val displayOptions = new FieldDisplayOptions(fieldSymbol,iconSource)
    
    //
    // Information on errors
    //
    
    val errorIfBlank : Boolean = hasAnnotation(typeErrorIfBlank)
    val errorIfNotRexex : Option[String] = annotationValueString(typeErrorIfNotRegex)
    val errorIfNotSuffix : Option[String] = annotationValueString(typeErrorIfNotSuffix)
    
    def escapeLiteralInRegex(literal:String) : String = literal.toList.map{"\\"+_}.mkString("")
    val expectedRegex : Option[String] = errorIfNotRexex.orElse(errorIfNotSuffix.map{".*"+escapeLiteralInRegex(_)})
    
    def errorCheckInfo(annotationClass:reflect.runtime.universe.Type) : Option[UniquenessCheckInfo] = {
      for (a<-annotation(annotationClass)) yield {
        val key = a.getString("key") match {
          case Some(skey) if skey!=null && !skey.isEmpty => skey
          case _ => this
        }
        new UniquenessCheckInfo(new UniquenessClass(key),a.getString("severity"))
      } 
    }
    
    val namesOfExpectedFields : Seq[String] = 
      if (isIndividuallyEditable) xsinfo.get.transitiveSubclasses.map{"add."+_.name}
      else if (isStringEditable || isBooleanEditable) List(name)
      else Nil
    // println(" param "+name+" type "+ptype+" isScalaCollection="+isScalaCollection+" isArray="+isArray+"  base type="+baseType)
      
    def textResources(locale:Locale) : TextLocalizationResources = new PrefixedTextLocalizationResources(parentInfo.textResources(locale),name+".")
  }

class FieldDisplayOptions(val field:reflect.runtime.universe.Symbol,iconSource:IconManifests) {
    def annotation(annotationClass:reflect.runtime.universe.Type,fieldName:String) : Option[Any] = SerializableTypeInfo.annotationField(field, annotationClass, fieldName)
    def hasAnnotation(annotationClass:reflect.runtime.universe.Type) = SerializableTypeInfo.hasAnnotation(field, annotationClass)
    def annotationString(annotationClass:reflect.runtime.universe.Type,fieldName:String) : Option[String] = annotation(annotationClass,fieldName).map{_.asInstanceOf[String]} 
    def annotationInt(annotationClass:reflect.runtime.universe.Type,fieldName:String,defaultValue:Int) : Option[Int] = annotation(annotationClass,fieldName).map{v=>if (v==null) defaultValue else v.asInstanceOf[Int]} 
    def annotationValueString(annotationClass:reflect.runtime.universe.Type) : Option[String] = annotationString(annotationClass,"value")
    def annotationValueStringArray(annotationClass:reflect.runtime.universe.Type) : Option[Seq[String]] = SerializableTypeInfo.getOptionalArrayStrings(field,annotationClass)
    def annotationValueInt(annotationClass:reflect.runtime.universe.Type,defaultValue:Int) : Option[Int] = annotationInt(annotationClass,"value",defaultValue) 

  
    val editSection :Option[String] = annotationValueString(typeEditSection)
    val orderingPriority : Int = getOptionalIntValue(field,typeOrderingPriority).getOrElse(0)
    val maxLength : Option[Int] = for (l<-annotationValueInt(typeMaxLength,0); if l>0) yield l
    val displayOnly : Boolean = hasAnnotation(typeDisplayOnly)
    /** Only meaningful if isStringEditable */
    val knownInterpretation : Option[String] = for (s<-annotationString(typeStringEditable,"knownInterpretation"); if s!=null && !s.isEmpty) yield s
    val multiline : Boolean = hasAnnotation(typeMultiline)
    val hideName : Boolean = hasAnnotation(typeHideName)
    val wholeLine : Boolean = hasAnnotation(typeWholeLine)
    val icon : Option[Icon] = for (logicalName<-annotationValueString(typeXSIcon); if logicalName!=null && !logicalName.isEmpty(); icon<-iconSource.iconOfLogicalName(logicalName)) yield icon

    val noBorder : Boolean = hasAnnotation(typeNoBorder)
}

class EnumeratedOptions(
    val required:Boolean,
    /** Identifiers for all the options */
    val options:Seq[String],
    /** Optional pretty printing for the options - map from identifiers to pretty values. Can be overrriden by localization. */
    val prettyPrint:Map[String,String],
    /** Optional icons for the options - map from identifiers to pretty values. Can be overrriden by localization. */
    val icons:Map[String,String],
    val classForLocalizationResources:Option[Class[_]]) {
  lazy val iconSource : Option[IconManifests] = classForLocalizationResources.map{IconManifests.getIconManifests(_)}
}

trait GeneralizedField {
  def name:String
}