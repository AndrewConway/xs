/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.impl

import org.greatcactus.xs.api.dependency.ExternalDependency
import org.greatcactus.xs.api.dependency.ExternalDependencyResolver
import org.greatcactus.xs.frontend.XSTreeNode
import org.greatcactus.xs.api.dependency.Parent
import scala.collection.mutable.ListBuffer
import scala.collection.Traversable
import org.greatcactus.xs.api.errors.XSError
import scala.collection.GenTraversable
import org.greatcactus.xs.frontend.DetailsPaneFields
import org.greatcactus.xs.api.errors.Severity
import org.greatcactus.xs.api.dependency.ExternallyChangingDependency
import org.greatcactus.xs.api.dependency.OnObsoleteCallback




class ExtraDisplayFieldInfo(val function:DependencyInjectionFunction,val name:String,val displayOptions:FieldDisplayOptions) {
  
}
class CustomFieldInfo(val function:DependencyInjectionFunction,val name:String,val displayOptions:FieldDisplayOptions,val customComponentName:String) {
  
}

class FunctionForField(val function:DependencyInjectionFunction,val field:Option[String]) {
  def fieldOrElseWhole : String = field.getOrElse(DetailsPaneFields.wholeFormAsFieldName)
}

/** Information on all the possible dependency injection functions, and all error functions. This is static, one instance per class */ 
class DependencyInjectionInformation(
  val providers : Seq[DependencyInjectionFunction],
  val iconProviders : Seq[FunctionForField],
  val labelProviders : Seq[FunctionForField],
  val extraText : Seq[ExtraDisplayFieldInfo],
  val customFields : Seq[CustomFieldInfo],
  val enabledControllers : Seq[FunctionForField],
  val visibilityControllers : Seq[FunctionForField],
  val errorChecks : Seq[FunctionForField],
  val kidFilter : CanPassToChildren,
  val simpleErrorChecks:SimpleErrorChecks
  ) {
  val allFunctions : Seq[DependencyInjectionFunction] = providers++(iconProviders.map{_.function})++(labelProviders.map{_.function})++(extraText.map{_.function})++(customFields.map{_.function})++(enabledControllers.map{_.function})++(visibilityControllers.map{_.function})++(errorChecks.map{_.function})
  val classLabelProvider : Option[DependencyInjectionFunction] = labelProviders.find{_.field==None}.map{_.function}
  val classIconProvider : Option[DependencyInjectionFunction] = iconProviders.find{_.field==None}.map{_.function}
  val fieldIconProvider : Map[String,DependencyInjectionFunction] = Map.empty++(for (ip<-iconProviders;f<-ip.field) yield f->ip.function)
  val fieldLabelProvider : Map[String,DependencyInjectionFunction] = Map.empty++(for (ip<-labelProviders;f<-ip.field) yield f->ip.function)
  val fieldEnabledController : Map[String,DependencyInjectionFunction] = Map.empty++(for (ip<-enabledControllers;f<-ip.field) yield f->ip.function)
  val fieldVisibilityController : Map[String,DependencyInjectionFunction] = Map.empty++(for (ip<-visibilityControllers;f<-ip.field) yield f->ip.function)
  val fromParentDependencyInfo = {
      /** True iff the "Parent" object class is ever used by a DependencyInjectionFunction */
     val couldEverUseParentObject : Boolean = allFunctions.exists{_.usesParentObject} 
     new FromParentDependencyInfo(couldEverUseParentObject)
  }
  val errorFunctionsByField : Map[String,Seq[DependencyInjectionFunction]] = Map.empty++(for ((field,fn)<-errorChecks.groupBy(_.fieldOrElseWhole)) yield field->(fn.map{_.function}))
  val fieldsThatCouldHaveErrors : Set[String] = errorFunctionsByField.keys.toSet++simpleErrorChecks.checks.keys
}


class FunctionEvaluationStatus(val function:DependencyInjectionFunction,val args:Seq[AnyRef],holder:DependencyInjectionCurrentStatus,obj:reflect.runtime.universe.InstanceMirror) {
  var callbackOnDispose : Option[()=>Unit] = None

  def onExternalChange() {
    callbackOnDispose=None; 
    holder.changedExternalResource(this)
  }
  
  /** The result of the function, or None if it executed with an error */
  val res: Option[AnyRef] = try {
    val argsWithChanges = for (a<-args) yield if (a==null) new OnObsoleteCallback(onExternalChange _) else a
    function.apply(obj,argsWithChanges) match {
      case null => None
      case e:ExternalDependency => holder.associatedNode.xsedit.externalDependencyResolver match {
        case Some(resolver) =>
          val resolved = resolver.resolve(e,onExternalChange _)
          callbackOnDispose = resolved.onNoLongerUsed
          Option(resolved.result)
        case None => None
      }
      case e:ExternallyChangingDependency =>
        callbackOnDispose = e.onNoLongerUsed
        Option(e.actual)
      case value => Some(value)
    }
  } catch { case e:Exception => None}
  
  val shouldInjectToKids = function.isInjectedToKids && res.isDefined && res.get!=null
  
  
  
  def dispose() { for (cb<-callbackOnDispose) cb() } // remove listeners
}

class CanPassToChildren(classesToBlockForChildren:Seq[Class[_]]) extends Function[AnyRef,Boolean] {
  def apply(x:AnyRef) : Boolean = {
    if (x==null) return false
    val c = x.getClass
    !classesToBlockForChildren.exists{_.isAssignableFrom(c)}
  }
}

class FromParentDependencyInfo(val couldEverUseParentObject:Boolean)

class ToChildDependencies(val base:Set[AnyRef],val parent:Parent[_]) {
  private lazy val withParent = if (parent==null) base else base+parent
  def get(ci:FromParentDependencyInfo) = {
    if (ci.couldEverUseParentObject) withParent
    else base
  }
}

object DependencyInjectionCurrentStatus {
  var debugDependencyInjections=false
}
/**
 * Information about dependency injection functions and error functions for one particular tree node object.
 * 
 * This may be relatively expensive to compute, so when you request data from it, it returns the last good data. It may
 * be changed subsequently; see DependencyInjectionCleaningQueue for details.
 */
class DependencyInjectionCurrentStatus(val info:DependencyInjectionInformation,val associatedNode:XSTreeNode) {
  private var dirty = true
  private var dirtySimpleErrorChecks = true
  private var injectedFromParent : Set[AnyRef] = associatedNode.injectionNodesFromParent;
  private var existingResolved : Map[DependencyInjectionFunction,FunctionEvaluationStatus] = Map.empty
  private var lastGoodResolved : Map[DependencyInjectionFunction,FunctionEvaluationStatus] = Map.empty
  private var sendToChildren : ToChildDependencies = new ToChildDependencies(Set.empty,null)
  private var parentMirror : reflect.runtime.universe.InstanceMirror = null
  private var parentObject : AnyRef = null
  private val errorListCache = new collection.mutable.HashMap[String,List[XSError]] // gets reset whenever lastGoodResolved is changed.
  private var worstErrorCache : Option[Int] = None // gets reset whenever lastGoodResolved is changed.
  private var simpleErrorCheckResults : Option[SimpleErrorCheckResults] = None // gets reset whenever invalid.
  
  def dependenciesToPropagateToChildren(ci:FromParentDependencyInfo) = sendToChildren.get(ci)
  
  def getIconSpec : Option[AnyRef] = for (f<-info.classIconProvider;fr<-lastGoodResolved.get(f); res <-fr.res) yield res
  def getIconSpecForField(fieldname:String) : Option[AnyRef] = for (f<-info.fieldIconProvider.get(fieldname);fr<-lastGoodResolved.get(f); res <-fr.res) yield res
  
  def getLabel : Option[AnyRef] = for (f<-info.classLabelProvider;fr<-lastGoodResolved.get(f); res <- fr.res ) yield res
  def getLabelForField(fieldname:String) : Option[AnyRef] = for (f<-info.fieldLabelProvider.get(fieldname);fr<-lastGoodResolved.get(f); res <- fr.res ) yield res
  
  def isEnabled(fieldname:String) : Boolean = (for (f<-info.fieldEnabledController.get(fieldname);fr<-lastGoodResolved.get(f); res <-fr.res if res==false) yield false).getOrElse(true)
  def isVisible(fieldname:String) : Boolean = (for (f<-info.fieldVisibilityController.get(fieldname);fr<-lastGoodResolved.get(f); res <-fr.res if res==false) yield false).getOrElse(true)
  
  def processErrorResults(result:AnyRef,found: XSError=>Unit) { result match {
          case null =>
          case None =>
          case Some(a:AnyRef) => processErrorResults(a,found)
          case e:XSError => found(e)
          case c:GenTraversable[_] => for (e<-c) processErrorResults(e.asInstanceOf[AnyRef],found)
          case a:Array[_] => for (e<-a) processErrorResults(e.asInstanceOf[AnyRef],found)
          // should have a LocalizableErrorDetails class
          case _ => throw new IllegalArgumentException("Error check code produced "+result)
        }}
  
  private def getSimpleErrors(fieldname:String) : List[XSError] = synchronized {
    //println("Getting simple errors for "+fieldname)
    if (simpleErrorCheckResults.isEmpty ) simpleErrorCheckResults=Some(info.simpleErrorChecks.check(associatedNode))
    simpleErrorCheckResults.get.errors.get(fieldname).getOrElse(Nil)
  }
  def getErrors(fieldname:String) : List[XSError] = {
    //println("getErrors("+fieldname+")")
    synchronized {
      errorListCache.getOrElseUpdate(fieldname,{
        var res = new ListBuffer[XSError]
        res++= getSimpleErrors(fieldname)
        def proc(result:AnyRef) { processErrorResults(result,res+= _) }
        for (functions<-info.errorFunctionsByField.get(fieldname);f<-functions; fr<-lastGoodResolved.get(f); frres<-fr.res) proc(frres)
        res.toList
      })
    }
  } 
  
  def worstErrorLevel : Int = synchronized {
    if (worstErrorCache.isEmpty) {
      var worst = 1000
      def proc(result:AnyRef) { processErrorResults(result,e=>worst=worst min e.severity.level()) }
      for (functionForField<-info.errorChecks;fr<-lastGoodResolved.get(functionForField.function); frres<-fr.res) proc(frres)
      for (name<-info.simpleErrorChecks.checks.keys;e<-getSimpleErrors(name)) worst=worst min e.severity.level() 
      worstErrorCache = Some(worst)
    }
    worstErrorCache.get
  }

  def changedUniquenessValues(ftc:FieldsToCheckForUniqueness) {
    synchronized {
      dirtySimpleErrorChecks=true
      worstErrorCache=None
      for (c<-ftc.classes;field<-c.fields) errorListCache-=field.name 
      simpleErrorCheckResults=None
    }
    associatedNode.xsedit.dependencyInjectionCleaningQueue.add(associatedNode)
  }
  
  def getFunctionResult(function:DependencyInjectionFunction) : Option[AnyRef] = lastGoodResolved.get(function).flatMap{_.res} 
  
  def dispose() {
    synchronized {
      for ((_,oldvalue)<-existingResolved) oldvalue.dispose()
    }
  }

  /**
   * Compute all dependencies, and send modified kids dependencies to children. Return true if anything may have changed.
   */
  def clean() = {
    synchronized {
      if (dirty && parentMirror!=null) {
        var resInjections = injectedFromParent
        var resResolved : Map[DependencyInjectionFunction,FunctionEvaluationStatus] = Map.empty
        var mustDo = info.allFunctions
        def addResolved(resolution:FunctionEvaluationStatus) {
          resolution.res match {
            case Some(result) if resolution.function.isLocallyInjected && result!=null => resInjections+=result
            case _ =>
          }
          resResolved+=resolution.function->resolution
        }
        def processed(f:DependencyInjectionFunction) : Boolean = {
          f.getArgs(resInjections) match {
            case Some(args) =>
              existingResolved.get(f) match {
                case Some(existing) if existing.args == args => addResolved(existing)
                case _ =>
                  addResolved(new FunctionEvaluationStatus(f,args,this,parentMirror))
              }
              true
            case None => false
          }
        }
        var finished=false
        while (!finished) {
          val (done,notDone) = mustDo.partition(processed(_))
          mustDo = notDone
          finished = notDone.isEmpty || done.isEmpty // former if all succeeded; latter if stuck.
        }
        for ((oldkey,oldvalue)<-existingResolved) if (resResolved.get(oldkey)!=Some(oldvalue)) oldvalue.dispose()
        existingResolved = resResolved
        lastGoodResolved = resResolved
        errorListCache.clear()
        worstErrorCache = None
        sendToChildren = new ToChildDependencies(injectedFromParent.filter(info.kidFilter).filter{! _.isInstanceOf[Parent[_]]}++existingResolved.values.filter{_.shouldInjectToKids}.map{_.res.get},new Parent(parentObject))
        for (c<-associatedNode.allChildren) c.dependencyInjection.changedParentInjections(sendToChildren.get(c.info.dependencyInjectionInfo.fromParentDependencyInfo))
        dirty=false
        if (DependencyInjectionCurrentStatus.debugDependencyInjections && !(lastGoodResolved.values.isEmpty && mustDo.isEmpty)) {
          println("Dependency Injection for "+parentObject)
          if (!lastGoodResolved.values.isEmpty) println(" Successfully resolved "+lastGoodResolved.values.map{_.function.name}.mkString(","))
          if (!mustDo.isEmpty) {
            println(" Unsuccessful resolution of "+mustDo.map{_.name}.mkString(","))
            println("  Available types :"+resInjections.toSeq.map{_.getClass.getSimpleName}.mkString(","))
          }
        }
        dirtySimpleErrorChecks=false
        true
      } else if (dirtySimpleErrorChecks) {
        dirtySimpleErrorChecks=false
        true
      }
      else false
    }
  }
  
  def changedObject(oldObject:AnyRef,newObject:AnyRef) {
    synchronized {
      existingResolved = existingResolved.filter{_._2.function.survivesChange(oldObject,newObject)}
      makeDirty()
      parentMirror = if (newObject==null) null else scala.reflect.runtime.currentMirror.reflect(newObject)
      parentObject = newObject
      simpleErrorCheckResults=None
    }
  }
  
  private def makeDirty() {
    if (!dirty) {
      dirty=true
      associatedNode.xsedit.dependencyInjectionCleaningQueue.add(associatedNode)
    }
  }
  def changedParentInjections(newInjectionsFromParent : Set[AnyRef]) {
    synchronized {
      if (newInjectionsFromParent != injectedFromParent) {
        injectedFromParent=newInjectionsFromParent;
        makeDirty()
      }      
    }
  }
  
  def changedExternalResource(obsoleted:FunctionEvaluationStatus) {
    synchronized {
      if (existingResolved.get(obsoleted.function)==Some(obsoleted)) {
        existingResolved-=obsoleted.function
        makeDirty()
        associatedNode.xsedit.dependencyInjectionCleaningQueue.cleanReturningInstantlyIfSomeOtherThreadIsAlreadyCleaning()
      }
    }
  }
  
}


class DependencyInjectionFunction(
    val argTypes:List[Class[_]],
    val method:scala.reflect.runtime.universe.MethodSymbol,
    val isLocallyInjected:Boolean,
    val isInjectedToKids:Boolean,
    val onlyAffectedByFields:Option[Seq[XSFieldInfo]]
    ) extends GeneralizedField {
  def apply(obj:reflect.runtime.universe.InstanceMirror,args:Seq[Any]) : AnyRef = {
    obj.reflectMethod(method).apply(args :_*).asInstanceOf[AnyRef]
  }
  
  def survivesChange(oldObject:AnyRef,newObject:AnyRef) = onlyAffectedByFields match {
    case Some(troublesomeFields) if oldObject!=null && newObject!=null => 
      troublesomeFields.forall{ field => field.getField(oldObject)==field.getField(newObject)}
    case _ => false // have to assume it is affected by everything
  }
  def getArgs(injections:Traversable[AnyRef]) : Option[Seq[AnyRef]] = {
    Some(for (t<-argTypes) yield {
      if (t == SerializableTypeInfo.classOnObsoleteCallback) null
      else injections.find{inj=>t.isAssignableFrom(inj.getClass())} match {
        case Some(injected) => injected
        case None => return None
      }
    })
  }
  val name:String = method.name.decoded
  def usesParentObject = argTypes.contains(classOf[Parent[_]])
}