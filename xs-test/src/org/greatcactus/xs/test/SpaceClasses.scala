/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.test

import org.greatcactus.xs.api.XS
import org.greatcactus.xs.api.XSSubclasses
import org.greatcactus.xs.api.edit._
import org.greatcactus.xs.api.dependency._
import org.greatcactus.xs.api.display._
import scala.collection.mutable.ListBuffer
import org.greatcactus.xs.api.errors._
import org.greatcactus.xs.api.icon.Icon

/**
 * Contains all the classes used in the space test framework.
 *
 */
@XS
@XSIcon("Space")
class Space(@IndividuallyEditable val history:History,@IndividuallyEditable val stars:List[Star]) {
  override def toString = "Space"
  @DependencyProvider @PropagateToChildren @OnlyAffectedByFields(Array("history")) def getHistoryInfo = new ExternalDependency(history.fileref,classOf[HistoryFile])
}

class HistoryFile(val lines:Array[String])

@XS
class History(@ErrorIfNotSuffix(".txt") @StringEditable val fileref:String) {
  @ExtraDisplayField def historyFileText(f:HistoryFile) = {
    val text = f.lines.mkString("\n")
    RichLabel(text, <pre>{text}</pre> )
  }
  override def toString = "History"
}


@XS
@XSSubclasses(Array(classOf[Star],classOf[Planet],classOf[Comet]))
abstract class HeavenlyBody {
  def name:String
}


@XS
@XSIcon("Star")
class Star(
    @ErrorIfNotUniqueInParent @ErrorIfBlank @StringEditable @DefaultValue("New star") val name:String,
    @StringEditable @Multiline val constellation:Option[String],
    @IndividuallyEditable val associated:List[HeavenlyBody]
  ) extends HeavenlyBody {
  override def toString = name
  
  @ExtraDisplayField @OnlyAffectedByFields(Array("name")) def nameReversed = if (name==null) null else name.reverse
  @ErrorCheck("name") private def checkName = ErrorChecks.checkValidCharacters(name)
  @ErrorCheck private def checkCometNumbers = if (associated.filter{_.isInstanceOf[Planet]}.length>associated.filter{_.isInstanceOf[Comet]}.length) Some(XSError.warning("More planets than comets")) else None
  @ExtraDisplayField @OnlyAffectedByFields(Array("constellation")) def constellationRaw = ""+constellation
  
  
}


@XS
@XSIcon("Planet")
class Planet(
    @ErrorIfNotGloballyUnique @StringEditable @DefaultValue("New planet") val name:String,
    @StringEditable val distanceFromPrimary:Double,
    @XSIcon("Planet") @StringEditable val tpe:PlanetType,
    @IndividuallyEditable val core:Option[Core],
    @IndividuallyEditable val moons:List[Planet],
    @BooleanEditable val tidallyLocked:Boolean,
    /*@StringEditable */ @ImageEditable val photo:String
  ) extends HeavenlyBody {
  override def toString = moons.size match {
    case 0 => name
    case 1 => name+" (1 moon)"
    case n => name+" ("+n+" moons)"
  } 
  @IconProvider private def planetTypeIcon = if (photo==null) planetTypeFieldIcon else Icon.ofURL(photo)
  @IconProvider("tpe") private def planetTypeFieldIcon = if (tpe==null) null else tpe.toString
  @LabelProvider private def fancyLabel = moons.size match {
    case 0 => name
    case 1 => RichLabel(name+" (1 moon)", <span>{name} <i>(1 moon)</i></span>)
    case n => RichLabel(name+" ("+n+" mons)", <span>{name} <i>({n} moons)</i></span>)
  } 
  @LabelProvider("distanceFromPrimary") private def fancyHomeLabel(parent:Parent[HeavenlyBody]) = "Distance from "+parent.get.name
  @VisibilityController("add.Planet") private def mayAddPlanet(parent:Parent[HeavenlyBody]) = parent.get.isInstanceOf[Star] 
  @EnabledController("tpe") private def mayAlterType(parent:Parent[HeavenlyBody]) = parent.get.isInstanceOf[Star] 
  @ErrorCheck("name") private def checkName = ErrorChecks.checkValidCharacters(name)
}

@XS
@XSIcon("Core")
class Core(@TableEditable val composition:List[CoreComposition]) {
  override def toString = "Core "+composition.mkString(" ")
  @ExtraDisplayField def sumPercent = composition.map{_.percent}.sum.toString
}

@XS
class CoreComposition(@StringEditable val element:String,@StringEditable val percent:Double,@BooleanEditable val magnetic:Boolean,@StringEditable @Multiline val comments:String,@StringEditable val evidence:Evidence) {
  override def toString = ""+percent+"% "+element
  @ErrorCheck("element") private def checkName = ErrorChecks.checkValidCharacters(element)
  @ErrorCheck("magnetic") private def checkMagnetic = if (magnetic != element.equalsIgnoreCase("fe")) XSError.warning("Are you sure?") else null
  @ErrorCheck("evidence") private def checkEvidence = if (evidence == Evidence.DirectMeasurement) XSError.error("You have got to be kidding!") else null
}

@XS
@XSIcon("Comet")
class Comet(
    @CustomPopup("DemoPopup") @EditSection("nomenclature") @StringEditable @DefaultValue("New comet")  val name:String,
    @StringEditable val period:Double
 ) extends HeavenlyBody {
  override def toString = name
}

object ErrorChecks {
  /** Do a check that every character in the string is a letter, a digit, or a space. */
  def checkValidCharacters(s:String) : List[XSError] = {
    val res = new ListBuffer[XSError]
    for (i<-0 until s.length) {
      val c=s(i)
      if (!(c==' '||c.isLetterOrDigit)) res+=XSError.error("Illegal character "+c,i,i+1)
    }
    res.toList
  }
}

