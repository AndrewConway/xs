/**
 * Copyright Andrew Conway 2013-2014. All rights reserved.
 */
package org.greatcactus.xs.impl

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
import org.greatcactus.xs.api.command.ProgressMonitor
import java.io.PrintWriter
import java.io.ByteArrayOutputStream
import java.io.StringWriter
import org.greatcactus.xs.api.command.CancelledThrowable
import org.greatcactus.xs.api.display.RichLabel
import org.greatcactus.xs.api.command.CommandResult
import java.lang.reflect.InvocationTargetException
import org.greatcactus.xs.api.dependency.IndexInParentField
import org.greatcactus.xs.frontend.ProgressMonitorInfo
import org.greatcactus.xs.frontend.ActionBusy
import scala.concurrent.ExecutionContext
import scala.collection.GenSeq
import scala.concurrent.Promise
import scala.concurrent.Future
import org.greatcactus.xs.frontend.html.SessionManagement
import org.greatcactus.xs.util.InterruptableFuture
import org.greatcactus.xs.util.ObsoletableAndInterruptableFuture
import org.greatcactus.xs.api.dependency.IndexInParentField
import org.greatcactus.xs.api.edit.UpdateField
import org.greatcactus.xs.api.display.Localizable
import java.util.Locale
import org.greatcactus.xs.api.display.TextLocalizationResources
import java.util.concurrent.ExecutionException


/** 
 * Manage context used by the dependency injection framework. A user could set this to
 * something else so that a servlet destruction event can close it down cleanly.
 */
object XSExecutionContext {
  var context : ExecutionContext = ExecutionContext.global 
}


class ExtraDisplayFieldInfo(val function:DependencyInjectionFunction,val name:String,val displayOptions:FieldDisplayOptions) {
  
}
class CommandMethod(val function:DependencyInjectionFunction,val name:String,val displayOptions:FieldDisplayOptions) {
  
}
class EditCommandMethods(val function:DependencyInjectionFunction,val name:String,val displayOptions:FieldDisplayOptions) {
  
}
class CustomFieldInfo(val function:DependencyInjectionFunction,val name:String,val displayOptions:FieldDisplayOptions,val customComponentName:String) {
  
}

class FunctionForField(val function:DependencyInjectionFunction,val field:Option[String]) {
  def fieldOrElseWhole : String = field.getOrElse(DetailsPaneFields.wholeFormAsFieldName)
}

/** 
 *  Information on all the possible dependency injection functions, and all error functions. This is static, one instance per class
 *  
 *  Some of these need to computed all the time - e.g. error checks.
 *  Some of these need to only be computed when the particular object is being displayed - e.g. labelProviders (except for class which goes into the tree)
 *  Some of these may also need to only be computed if a descendent is being displayed, but there is no way to express this at the moment.
 **/ 
class DependencyInjectionInformation(
  val providers : Seq[DependencyInjectionFunction],
  val iconProviders : Seq[FunctionForField],
  val labelProviders : Seq[FunctionForField],
  val tooltipProviders : Seq[FunctionForField],
  val extraText : Seq[ExtraDisplayFieldInfo],
  val editCommands : Seq[EditCommandMethods],
  val customFields : Seq[CustomFieldInfo],
  val enabledControllers : Seq[FunctionForField],
  val visibilityControllers : Seq[FunctionForField],
  val errorChecks : Seq[FunctionForField],
  val kidFilter : CanPassToChildren,
  val simpleErrorChecks:SimpleErrorChecks,
  val commands:List[CommandMethod]
  ) {
  val allDIFunctions : Seq[DependencyInjectionFunction] = providers++(iconProviders.map{_.function})++(labelProviders.map{_.function})++(tooltipProviders.map{_.function})++(extraText.map{_.function})++(editCommands.map{_.function})++(customFields.map{_.function})++(enabledControllers.map{_.function})++(visibilityControllers.map{_.function})++(errorChecks.map{_.function})
  private def classfns(l:Seq[FunctionForField]) = l.filter{_.field.isEmpty}.map{_.function}
  val allDIFunctionsNeededEvenIfNotVisible : Seq[DependencyInjectionFunction] = providers++classfns(iconProviders)++classfns(labelProviders)++classfns(tooltipProviders)++(errorChecks.map{_.function}) 
  val allFunctions = allDIFunctions++(commands.map{_.function})
  val classLabelProvider : Option[DependencyInjectionFunction] = labelProviders.find{_.field==None}.map{_.function}
  val classTooltipProvider : Option[DependencyInjectionFunction] = tooltipProviders.find{_.field==None}.map{_.function}
  val classIconProvider : Option[DependencyInjectionFunction] = iconProviders.find{_.field==None}.map{_.function}
  val fieldIconProvider : Map[String,DependencyInjectionFunction] = Map.empty++(for (ip<-iconProviders;f<-ip.field) yield f->ip.function)
  val fieldLabelProvider : Map[String,DependencyInjectionFunction] = Map.empty++(for (ip<-labelProviders;f<-ip.field) yield f->ip.function)
  val fieldTooltipProvider : Map[String,DependencyInjectionFunction] = Map.empty++(for (ip<-tooltipProviders;f<-ip.field) yield f->ip.function)
  val fieldEnabledController : Map[String,DependencyInjectionFunction] = Map.empty++(for (ip<-enabledControllers;f<-ip.field) yield f->ip.function)
  val fieldVisibilityController : Map[String,DependencyInjectionFunction] = Map.empty++(for (ip<-visibilityControllers;f<-ip.field) yield f->ip.function)
  val fromParentDependencyInfo = {
      /** True iff the "Parent" object class is ever used by a DependencyInjectionFunction */
     val couldEverUseParentObject : Boolean = allFunctions.exists{_.usesParentObject}
     val couldEverUseIndexInParentField : Boolean = allFunctions.exists{_.usesIndexInParentField}
     new FromParentDependencyInfo(couldEverUseParentObject,couldEverUseIndexInParentField)
  }
  val errorFunctionsByField : Map[String,Seq[DependencyInjectionFunction]] = Map.empty++(for ((field,fn)<-errorChecks.groupBy(_.fieldOrElseWhole)) yield field->(fn.map{_.function}))
  val fieldsThatCouldHaveErrors : Set[String] = errorFunctionsByField.keys.toSet++simpleErrorChecks.checks.keys
}


class FunctionEvaluationStatus(val function:DependencyInjectionFunction,val args:Seq[AnyRef],holder:DependencyInjectionCurrentStatus,obj:reflect.runtime.universe.InstanceMirror) { festhis =>
  private[this] var callbackOnDispose : Option[()=>Unit] = None
  private[this] var isExternallyChanged = false

  def onExternalChange() {
    val notifyHolder = synchronized {
      if (!isExternallyChanged) {
        isExternallyChanged=true
        for (f<-callbackOnDispose) f()
        callbackOnDispose=None // not really necessary, but could help GC
        true
      } else false
    }
    if (notifyHolder) holder.changedExternalResource(this)    
  }
  def setCallbackOnDispose(f:()=>Unit) {
    synchronized {
      if (isExternallyChanged) f()
      else callbackOnDispose=Some(f)
    }
  }
  
  /** The result of the function, or None if it executed with an error */
  val rawres: Option[AnyRef] = try {
    val argsWithChanges = for (a<-args) yield if (a==null) new OnObsoleteCallback(onExternalChange _) else a
    function.apply(obj,argsWithChanges) match {
      case null => None
      case e:ExternallyChangingDependency =>
        setCallbackOnDispose(e.onNoLongerUsed)
        Option(e.actual)
      case e:ObsoletableAndInterruptableFuture[_] =>
        callbackOnDispose = Some(e.dispose)
        e.addChangeListener(onExternalChange)
        Option(e.future)
      case value => Some(value)
    }
  } catch { case e:Exception => e.printStackTrace(); None}
  
  private[this] val completedFutureSync = new Object
  private[this] var completedFuture : Option[AnyRef] = rawres
  private[this] var updateOnFutureGUI : Set[XSTreeNode] = Set.empty
  private[this] var updateOnFutureGUIincludingErrorLevels: Set[XSTreeNode] = Set.empty
  private[this] var updateOnFutureGUIIncludingTableFieldsCache : Set[XSTreeNode] = Set.empty
  private[this] var updateOnFutureDirtyable : Set[DependencyInjectionCurrentStatus] = Set.empty
  
  private[this] def dealWithFuture(f:Future[_]) {
    f.onSuccess{
      case newres => 
        if (holder.debug) println("Function "+function.name+" resolved as a future to "+newres)
        val todoOutsideOfSynchronized = new ListBuffer[() => Unit] // to prevent deadlock
        completedFutureSync.synchronized {
          completedFuture = Some(newres.asInstanceOf[AnyRef])
          for (n<-updateOnFutureGUI) todoOutsideOfSynchronized+= n.updateGUI
          updateOnFutureGUI=Set.empty   
          for (n<-updateOnFutureGUIincludingErrorLevels) todoOutsideOfSynchronized+= n.updateGUIincludingErrorLevels
          updateOnFutureGUIincludingErrorLevels=Set.empty   
          for (n<-updateOnFutureGUIIncludingTableFieldsCache) todoOutsideOfSynchronized+= n.updateGUIIncludingTableFieldsCache
          updateOnFutureGUIIncludingTableFieldsCache=Set.empty   
          for (dirtyable<-updateOnFutureDirtyable) {
            if (holder.debug) println("Recomputing dependency injections as future resolved for "+function.name)
            todoOutsideOfSynchronized+= dirtyable.makeDirty
            todoOutsideOfSynchronized+= dirtyable.associatedNode.xsedit.dependencyInjectionCleaningQueue.cleanReturningInstantlyIfSomeOtherThreadIsAlreadyCleaning
          }
          updateOnFutureDirtyable=Set.empty   
        }
        XSExecutionContext.context.execute(new Runnable() {override def run() {for (todo<-todoOutsideOfSynchronized) todo()}}) // done in a separate thread in case the thing calling this has lots of locks.
    }(XSExecutionContext.context)
    f.onFailure{
      case can:CancelledThrowable => 
      case _ : InterruptedException =>
      case e : ExecutionException if e.getCause().isInstanceOf[InterruptedException] =>
      case e:Throwable => println("Error in future "); e.printStackTrace()
    }(XSExecutionContext.context)
    holder.associatedNode.xsedit.dependencyInjectionCleaningQueue.addPendingFuture(festhis)
    f.onComplete{case _ => holder.associatedNode.xsedit.dependencyInjectionCleaningQueue.removePendingFuture(festhis)}(XSExecutionContext.context)
  }
  rawres match {
    case Some(p:Future[_]) => completedFuture=None; dealWithFuture(p) 
    case Some(p:InterruptableFuture[_]) => completedFuture=None; dealWithFuture(p.future) 
    case _ =>
  }

  
  def currentlyAwaitingFuture : Boolean = completedFuture.isEmpty && rawres.isDefined
  
  
  
  /** Get a result. If it may change later due to a future being completed, then call node.updateGUI() when that happens */
  def resForSimpleGUI(node:XSTreeNode) = {
    completedFutureSync.synchronized {
      if (currentlyAwaitingFuture) updateOnFutureGUI+=node
      completedFuture  
    }
  }
  /** Get a result. If it may change later due to a future being completed, then call node.updateGUIincludingErrorLevels() when that happens */
  def resForErrors(node:XSTreeNode) = {
    completedFutureSync.synchronized {
      if (currentlyAwaitingFuture) updateOnFutureGUIincludingErrorLevels+=node
      completedFuture  
    }
  }
  /** Get a result. If it may change later due to a future being completed, then call node.updateGUIIncludingTableFieldsCache() when that happens */
  def resForGUIAndTableFields(node:XSTreeNode) = {
    completedFutureSync.synchronized {
      if (currentlyAwaitingFuture) updateOnFutureGUIIncludingTableFieldsCache+=node
      completedFuture  
    }
  }
  
  /** Get a result. If it may change later due to a future being completed, then call node.updateGUIIncludingTableFieldsCache() when that happens */
  def resAsListForGUIAndTableFields(node:XSTreeNode) : List[AnyRef] = {
    completedFutureSync.synchronized {
      if (currentlyAwaitingFuture) updateOnFutureGUIIncludingTableFieldsCache+=node
      flattenCollections(completedFuture)
    }
  }
  
  //def res : Option[AnyRef] = completedFuture
  
  def resAsList(dirtyable:DependencyInjectionCurrentStatus) : List[AnyRef] = {
    completedFutureSync.synchronized {
      if (currentlyAwaitingFuture) updateOnFutureDirtyable+=dirtyable
      flattenCollections(completedFuture)
    }
  }
  
  def flattenCollections(x:Any) : List[AnyRef] = x match {
    case null => Nil
    case None => Nil
    case Some(o) => flattenCollections(o)
    case a:Array[_] => a.toList.flatMap{flattenCollections _}
    case c:GenSeq[_] => c.toList.flatMap{flattenCollections _}
    case x => List(x.asInstanceOf[AnyRef])
  }
  
  def shouldInjectToKids = function.isInjectedToKids && completedFuture.isDefined && completedFuture.get!=null
  
  
  
  def dispose() { 
    //println("Disposing of function status")
    for (cb<-callbackOnDispose) cb() // remove listeners
    rawres match {
      case Some(f:InterruptableFuture[_]) => f.cancel() 
      case _ =>
    }
  } 
}

class CanPassToChildren(classesToBlockForChildren:Seq[Class[_]]) extends Function[AnyRef,Boolean] {
  def apply(x:AnyRef) : Boolean = {
    if (x==null) return false
    val c = x.getClass
    !classesToBlockForChildren.exists{_.isAssignableFrom(c)}
  }
}

class FromParentDependencyInfo(val couldEverUseParentObject:Boolean,val couldEverUseIndexInParentField:Boolean)

class ToChildDependencies(val base:Set[AnyRef],val parent:Parent[_]) {
  private lazy val withParent = if (parent==null) base else base+parent
  def get(ci:FromParentDependencyInfo,indexInParent:Int) = {
    val res1 = if (ci.couldEverUseParentObject) withParent else base
    val res2 = if (ci.couldEverUseIndexInParentField) res1+(new IndexInParentField(indexInParent)) else res1 
    res2
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
  //private var dirty = true
  //private var dirtySimpleErrorChecks = true
  private var injectedFromParent : Set[AnyRef] = associatedNode.injectionNodesFromParent;
  private var existingResolved : Map[DependencyInjectionFunction,FunctionEvaluationStatus] = Map.empty
  private var lastGoodResolved : Map[DependencyInjectionFunction,FunctionEvaluationStatus] = Map.empty
  private var sendToChildren : ToChildDependencies = new ToChildDependencies(Set.empty,null)
  private var parentMirror : reflect.runtime.universe.InstanceMirror = null
  private var parentObject : AnyRef = null
  private val errorListCache = new collection.mutable.HashMap[String,List[XSError]] // gets reset whenever lastGoodResolved is changed.
  private var worstErrorCache : Option[Int] = None // gets reset whenever lastGoodResolved is changed.
  private var simpleErrorCheckResults : Option[SimpleErrorCheckResults] = None // gets reset whenever invalid.
  
  def dependenciesToPropagateToChildren(ci:FromParentDependencyInfo,indexInParent:Int) = sendToChildren.get(ci,indexInParent)
  
  def getIconSpec(node:XSTreeNode) : Option[AnyRef] = for (f<-info.classIconProvider;fr<-lastGoodResolved.get(f); res <-fr.resForSimpleGUI(node)) yield res
  def getIconSpecForField(fieldname:String,node:XSTreeNode) : Option[AnyRef] = for (f<-info.fieldIconProvider.get(fieldname);fr<-lastGoodResolved.get(f); res <-fr.resForSimpleGUI(node)) yield res
  
  def getLabel(node:XSTreeNode) : Option[AnyRef] = for (f<-info.classLabelProvider;fr<-lastGoodResolved.get(f); res <- fr.resForSimpleGUI(node) ) yield res
  def getLabelForField(fieldname:String,node:XSTreeNode) : Option[AnyRef] = for (f<-info.fieldLabelProvider.get(fieldname);fr<-lastGoodResolved.get(f); res <- fr.resForSimpleGUI(node) ) yield res
  def getTooltip(node:XSTreeNode) : Option[AnyRef] = for (f<-info.classTooltipProvider;fr<-lastGoodResolved.get(f); res <- fr.resForSimpleGUI(node) ) yield res
  def getTooltipForField(fieldname:String,node:XSTreeNode) : Option[AnyRef] = {
    val programmatic = for (f<-info.fieldTooltipProvider.get(fieldname);fr<-lastGoodResolved.get(f); res <- fr.resForSimpleGUI(node) ) yield res
    programmatic.orElse{
      // see if there are other ways of making a tooltip
      //println("Trying localizations for "+fieldname)
      associatedNode.info.getOptionalField(fieldname).flatMap{field=>
        // if the field is an enumeration, see if there are tooltips in the localization information
        for (options<-field.fixedOptions; cl<-options.classForLocalizationResources) {
          //println("Found fixed options "+fieldname)
          return Some(new Localizable{
            override def localize(locale:Locale) : String = {
              val text = TextLocalizationResources.getCached(locale, cl)
              val fieldValue = field.getFieldAsString(node.getObject)
              val res = if (fieldValue==null) null else text.get(fieldValue+".tooltip").getOrElse(null)
              //println("Trying localizations for "+fieldValue+" of type "+field.baseClassName+" found "+res)
              res
            }
          })
        }
        None
      }
    }
  }
  
  def isEnabled(fieldname:String,node:XSTreeNode) : Boolean = info.fieldEnabledController.get(fieldname) match {
    case Some(f) => (for (fr<-lastGoodResolved.get(f); res <-fr.resForSimpleGUI(node) if res==true) yield true).getOrElse(false)
    case None => true
  }
  def isVisible(fieldname:String,node:XSTreeNode) : Boolean = info.fieldVisibilityController.get(fieldname) match {
    case Some(f) => (for (fr<-lastGoodResolved.get(f); res <-fr.resForSimpleGUI(node) if res==true) yield true).getOrElse(false)
    case None => true
  }
  
  def processErrorResults(result:Any,found: XSError=>Unit,function:DependencyInjectionFunction) { result match {
          case null =>
          case None =>
          case Some(a) => processErrorResults(a,found,function)
          case e:XSError => found(e)
          case c:GenTraversable[_] => for (e<-c) processErrorResults(e,found,function)
          case a:Array[_] => for (e<-a) processErrorResults(e,found,function)
          // should have a LocalizableErrorDetails class
          case _ =>
            println("***** ERROR ** Error check function "+function.name+"(...) produced "+result)
            // throw new IllegalArgumentException("Error check function "+function.name+"(...) produced "+result)
        }}
  
  private def getSimpleErrors(fieldname:String) : List[XSError] = synchronized {
    //println("Getting simple errors for "+fieldname)
    if (simpleErrorCheckResults.isEmpty ) simpleErrorCheckResults=Some(info.simpleErrorChecks.check(associatedNode))
    simpleErrorCheckResults.get.errors.get(fieldname).getOrElse(Nil)
  }
  def getErrors(fieldname:String,node:XSTreeNode) : List[XSError] = {
    //println("getErrors("+fieldname+")")
    synchronized {
      errorListCache.getOrElseUpdate(fieldname,{
        var res = new ListBuffer[XSError]
        res++= getSimpleErrors(fieldname)
        def proc(result:AnyRef,function:DependencyInjectionFunction) { processErrorResults(result,res+= _,function) }
        for (functions<-info.errorFunctionsByField.get(fieldname);f<-functions; fr<-lastGoodResolved.get(f); frres<-fr.resForErrors(node)) proc(frres,f)
        res.toList
      })
    }
  } 
  
  def worstErrorLevel(node:XSTreeNode) : Int = synchronized {
    if (worstErrorCache.isEmpty) {
      var worst = 1000
      def proc(result:AnyRef,function:DependencyInjectionFunction) { processErrorResults(result,e=>worst=worst min e.severity.level(),function) }
      for (functionForField<-info.errorChecks;fr<-lastGoodResolved.get(functionForField.function); frres<-fr.resForErrors(node)) proc(frres,functionForField.function)
      for (name<-info.simpleErrorChecks.checks.keys;e<-getSimpleErrors(name)) worst=worst min e.severity.level() 
      worstErrorCache = Some(worst)
    }
    worstErrorCache.get
  }

  def changedUniquenessValues(ftc:FieldsToCheckForUniqueness) {
    synchronized {
      dirtyStatus.makeSimpleErrorChecksDirty()
      worstErrorCache=None
      for (c<-ftc.classes;field<-c.fields) errorListCache-=field.name 
      simpleErrorCheckResults=None
    }
    associatedNode.xsedit.dependencyInjectionCleaningQueue.add(associatedNode)
  }
  
  def getFunctionResult(function:DependencyInjectionFunction,node:XSTreeNode) : Option[AnyRef] = lastGoodResolved.get(function).flatMap{_.resForGUIAndTableFields(node)} 
  def getFunctionResultAsList(function:DependencyInjectionFunction,node:XSTreeNode) : List[AnyRef] = lastGoodResolved.get(function) match { case None => Nil; case Some(r) => r.resAsListForGUIAndTableFields(node)} 
  
  def dispose() {
    discardDependencies()
  }

  def discardDependencies() {
    synchronized {
      for ((_,oldvalue)<-existingResolved) oldvalue.dispose()
    }    
  }
  
  class DirtyHolder {
      private var dirty = true
      private var dirtyThingsOnlyNeededOnSelectedObject = true
      private var dirtySimpleErrorChecks = true
      private var hasBecomeDirtyDuringClean = false
      
      def isDirty(allNeeded:Boolean) = synchronized { if (allNeeded) dirtyThingsOnlyNeededOnSelectedObject else dirty }
      /** Clear dirty error checks, returning the value prior to clearing */
      def clearErrorChecksDirtiness() = synchronized { val old = dirtySimpleErrorChecks; dirtySimpleErrorChecks=false; old }
      def clean(doneAll:Boolean) { synchronized { dirty=false; dirtySimpleErrorChecks=false; if (doneAll) dirtyThingsOnlyNeededOnSelectedObject=false  } }
      def makeDirty() = synchronized { dirty=true; dirtyThingsOnlyNeededOnSelectedObject=true }
      def makeSimpleErrorChecksDirty() = synchronized { dirtySimpleErrorChecks=true; }
  }
  
  val dirtyStatus =new DirtyHolder
  
  /** Result of the last call to clean(). You usually do NOT want to use this. */
  var lastInjections : Set[AnyRef] = Set.empty // the result of the last call to clean()
  
  def debug : Boolean = DependencyInjectionCurrentStatus.debugDependencyInjections || associatedNode.info.debugDependencies
  /**
   * Compute all dependencies, and send modified kids dependencies to children. Return true if anything may have changed.
   */
  def clean() : Boolean = {
    synchronized {
      val shouldDoAll = associatedNode.isCurrentlyBeingEdited
      if (!dirtyStatus.isDirty(shouldDoAll)) return dirtyStatus.clearErrorChecksDirtiness()
      dirtyStatus.clean(shouldDoAll)
      if (parentMirror!=null) {
        var resInjections : Set[AnyRef] = injectedFromParent+XSExecutionContext.context
        var resResolved : Map[DependencyInjectionFunction,FunctionEvaluationStatus] = Map.empty
        var mustDo = if (shouldDoAll) info.allDIFunctions else info.allDIFunctionsNeededEvenIfNotVisible
        def addResolved(resolution:FunctionEvaluationStatus) {
          if (resolution.function.isLocallyInjected) resInjections++=resolution.resAsList(this)
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
        sendToChildren = new ToChildDependencies(injectedFromParent.filter(info.kidFilter).filter{case _:Parent[_] => false; case _:IndexInParentField => false; case _ => true}++existingResolved.values.filter{_.shouldInjectToKids}.flatMap{_.resAsList(this)},new Parent(parentObject))
        val _ = {
          var fieldInParent : XSFieldInfo = null
          var indexInParentField = 0
          for (c<-associatedNode.allChildren) {
            if (c.fieldInParent eq fieldInParent) indexInParentField+=1 else { fieldInParent=c.fieldInParent; indexInParentField=0 } 
            // println("fieldInParent="+fieldInParent+" c.fieldInParent="+c.fieldInParent+" indexInParent="+indexInParentField)
            c.dependencyInjection.changedParentInjections(sendToChildren.get(c.info.dependencyInjectionInfo.fromParentDependencyInfo,indexInParentField))
          }
        }
        //dirty=false
        if (debug && !(lastGoodResolved.values.isEmpty && mustDo.isEmpty)) {
          println("Dependency Injection for "+parentObjectName)
          if (!lastGoodResolved.values.isEmpty) println(" Successfully resolved "+lastGoodResolved.values.map{_.function.name}.mkString(","))
          if (!mustDo.isEmpty) {
            println(" Unsuccessful resolution of "+mustDo.map{_.name}.mkString(","))
            println("  Available types :"+resInjections.toSeq.map{_.getClass.getSimpleName}.mkString(","))
          }
        }
        lastInjections=resInjections
        //dirtySimpleErrorChecks=false
        for (dep<-resInjections) dep match {
          case changeField:UpdateField =>
            associatedNode.updateField(changeField)
          case _ =>
        }
        true
      } else false
    }
  }
  
  def executeCommandInSeparateThread(function:DependencyInjectionFunction,getMonitor : ProgressMonitorInfo,executionContext:ExecutionContext) { synchronized {
    clean()
    val monitor = try { getMonitor.getMonitor() } catch { case _:ActionBusy => return}
    val monitorSource = monitor.subTask(1.0)
    val possibleArgs = monitor::monitorSource::lastGoodResolved.toList
    function.getArgs(possibleArgs) match {
      case Some(args) =>
        //println("Got args")
        //import concurrent.ExecutionContext.Implicits.global
        val mirror = parentMirror
        concurrent.Future{
          def err(e:Throwable) = e match {
            case _:CancelledThrowable => monitor.failed(Some(RichLabel("Cancelled")))
            case _ =>
              e.printStackTrace()
              val os = new StringWriter
              val pw = new java.io.PrintWriter(os)
              e.printStackTrace(pw)
              pw.flush
              val text = os.toString
              monitor.failed(Some(RichLabel(text, <pre>{text}</pre>)))            
          }
          try {
            //println("in future")
            function.apply(mirror, args) match {
              case res:CommandResult => 
                if (res.success) monitor.succeeded(res.info)
                else monitor.failed(res.info)
              case _ => monitor.succeeded(None)
            }           
          } catch {
            case _:CancelledThrowable => monitor.failed(Some(RichLabel("Cancelled")))
            case e:InvocationTargetException => err(e.getTargetException())
            case e:Exception => err(e)
          }
          getMonitor.releaseMonitor()
        }(executionContext)
      case None => 
        //println("executeCommandInSeparateThread could not get args")
        monitor.failed(Some(RichLabel("Could not run command")))
        getMonitor.releaseMonitor()
    }
    
  }}
  
  def changedObject(oldObject:AnyRef,newObject:AnyRef) {
    synchronized {
      val (keepExisting,disposeExisting) = existingResolved.partition{_._2.function.survivesChange(oldObject,newObject)}
      existingResolved = keepExisting
      for ((_,d)<-disposeExisting) d.dispose()
      if (debug) println("Changed object "+makeNameSane(""+oldObject)+" to "+makeNameSane(""+newObject)+" saved "+existingResolved.size)
      //(new IllegalArgumentException).printStackTrace()
      makeDirty()
      parentMirror = if (newObject==null) null else scala.reflect.runtime.currentMirror.reflect(newObject)
      parentObject = newObject
      simpleErrorCheckResults=None
    }
  }
  
  def makeNameSane(s:String) = if (s==null) "" else s.replaceAll("[\r\n]+[\\s\\S]*","...")
  def parentObjectName = makeNameSane(parentObject.toString)
  private[impl] def makeDirty() { // cannot be synchronized as can lead to thread deadlocks.
    //println("Set to dirty")
    dirtyStatus.makeDirty()
    associatedNode.xsedit.dependencyInjectionCleaningQueue.add(associatedNode)
  }
  def changedParentInjections(newInjectionsFromParent : Set[AnyRef]) {
    if (debug) println("Changed injections from parent for "+parentObjectName+" added "+(newInjectionsFromParent--injectedFromParent)+" removed "+(injectedFromParent--newInjectionsFromParent))
    synchronized {
      if (newInjectionsFromParent != injectedFromParent) {
        injectedFromParent=newInjectionsFromParent;
        makeDirty()
      }      
    }
  }
  
  def changedExternalResource(obsoleted:FunctionEvaluationStatus) {
    val needclean = synchronized {
      if (existingResolved.get(obsoleted.function)==Some(obsoleted)) {
        existingResolved-=obsoleted.function
        if (debug) println("Changed external resource for "+parentObject)
        makeDirty()
        true
      } else false
    }
    if (needclean) associatedNode.xsedit.dependencyInjectionCleaningQueue.cleanReturningInstantlyIfSomeOtherThreadIsAlreadyCleaning()
  }
  
}


class DependencyInjectionFunction(
    val argTypes:List[Class[_]],
    val method:scala.reflect.runtime.universe.MethodSymbol,
    val isLocallyInjected:Boolean,
    val isInjectedToKids:Boolean,
    val onlyAffectedByFields:Option[Seq[XSFieldInfo]]
    ) extends GeneralizedField {
  
  val javaMethod = DependencyInjectionFunction.javaMethod(method)
  
  def apply(obj:reflect.runtime.universe.InstanceMirror,args:Seq[AnyRef]) : AnyRef = {
    // reflection is not thread safe in scala 2.10. I think it is fixed in 2.11. So we need to wrap this is a global synchronized object :-(. 
    // obj.reflectMethod(method).apply(args :_*).asInstanceOf[AnyRef]
    // so use java reflection instead.
    javaMethod.invoke(obj.instance,args.toArray :_*)
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
  val name:String = method.name.decodedName.toString
  def usesParentObject = argTypes.contains(classOf[Parent[_]])
  def usesIndexInParentField = argTypes.contains(classOf[IndexInParentField])
  override def couldHaveImplicitTooltip = false
}

object DependencyInjectionFunction {
  // method of converting from scala reflection method to java method. This is an internal hack as there is no public api for it. It is important as the scala reflection API is not thread safe in 2.10, and putting a global sync could kill scalability.
  val cmx = scala.reflect.runtime.currentMirror.asInstanceOf[{
    def methodToJava(sym: scala.reflect.internal.Symbols#MethodSymbol): java.lang.reflect.Method
  }]
  
  import scala.language.reflectiveCalls
  def javaMethod(foo:scala.reflect.runtime.universe.MethodSymbol): java.lang.reflect.Method = {
    val res = cmx.methodToJava(foo.asInstanceOf[scala.reflect.internal.Symbols#MethodSymbol])
    res.setAccessible(true);
    res
  } 
  
  
}