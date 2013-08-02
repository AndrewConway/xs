/**
 * Copyright (c) 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.api.command

import org.greatcactus.xs.api.display.RichLabel
import org.greatcactus.xs.util.ObsoletableAndInterruptableFuture
import scala.concurrent.Await
import scala.concurrent.ExecutionContext

/**
 * Update the UI of a progress monitor
 */
trait ProgressMonitorUI {
  def donePortion(progressPortion:Double)  
  def failed(message:Option[RichLabel])
  def succeeded(message:Option[RichLabel])
}

/**
 * More friendly version of a progress monitor, adjusting units, allowing subtasks, and allowing subtasks with unknown extents.
 */
class ProgressMonitor private[command](ui:Option[ProgressMonitorUI],expectedWork:Option[Double],parent:Option[ProgressMonitor]) {
  private var childPortionWork : Double = _;
  private[command] var child : Option[ProgressMonitor] = None
  private var workDone = 0.0
  private var workDoneInChild = 0.0
  private var lastPortionSentUp = Double.NaN
  private var hasBeenCancelled = false
  private[this] var notifyOfCancel : List[() => Unit] = Nil
  
  def addNotifyOfCancel(callback: ()=>Unit) {
    parent match {
      case Some(p) => p.addNotifyOfCancel(callback)
      case None => synchronized { notifyOfCancel = callback::notifyOfCancel }
    }
  }
  /** Called by the UI to indicate that it has been cancelled */
  def cancel() {
    //println("Job has been cancelled")
    val toNotify = synchronized {
      hasBeenCancelled=true;
      val res = notifyOfCancel
      notifyOfCancel=Nil
      res
    }
    for (f<-toNotify) f()
    for (p<-parent) p.cancel()
  }

  def failed(message:Option[RichLabel])  {
    for (p<-parent) p.failed(message)
    for (u<-ui) u.failed(message)
  }
  def succeeded(message:Option[RichLabel])  {
    for (p<-parent) p.succeeded(message)
    for (u<-ui) u.succeeded(message)
  }

  /** Called by the algorithm to see if should stop */
  def isCancelled : Boolean = hasBeenCancelled || parent.isDefined && parent.get.isCancelled
  /** Called by the algorithm periodically to see if should stop due to being cancelled, and throws and exception if so */
  def checkCancelled() { if (isCancelled) throw new CancelledThrowable }
  /**
   * Call this when starting a subtask. It produces a progress monitor for that subtask.
   * expectedWork is the total amount of work done in the subtask. It will be rescaled to...
   * portionOfParent is the portion of the current progress monitor that the subtask is.
   */
  def startTask(name:String,expectedWork:Option[Double],portionOfParent:Double=1.0,workUnits:Option[String]=None) : ProgressMonitor = {
    for (c<-child) c.finished()
    val res = new ProgressMonitor(None,expectedWork,Some(this))
    child = Some(res)
    childPortionWork=portionOfParent
    workDoneInChild=0.0
    res
  }
  /** More common method - makes a stub that a receiving function can use to set Task appropriately. This is used at the call site, and the get method is used at the start of the called function and together they are the same as startTask. */
  def subTask(portionOfParent:Double) : ProgressMonitorSource = {
    for (c<-child) c.finished()
    val res = new ProgressMonitor(None,expectedWork,Some(this))
    child = None
    childPortionWork=portionOfParent
    workDoneInChild=0.0
    new ProgressMonitorSource(None,Some(this))
  }
  private def childHasFinished() {
    childHasDoneWork(1.0)
    child=None
  }
  private def childHasDoneWork(childPortionDone:Double) {
    doUnitWork((childPortionDone-workDoneInChild)*childPortionWork)
    workDoneInChild=childPortionDone;
  }
  /** The proportion of completed work */
  def donePortion : Double = {
    expectedWork match {
      case Some(expectedMax) => (workDone/expectedMax) min 1.0
      case None => 1.0-20.0/(20.0+workDone)
    }
  }
  
  /** Increase the amount of work by some increment  */
  def doUnitWork(amount:Double=1.0) {
    workDone+=amount;
    val newportion = donePortion
    if (!(lastPortionSentUp>newportion+0.01)) { // has to have the negative to make work with NaN
      lastPortionSentUp=newportion
      for (p<-parent) p.childHasDoneWork(newportion)
      for (u<-ui) u.donePortion(newportion)
    } 
    checkCancelled()
  }
  /** Finished this task or subtask */
  def finished() {
    for (p<-parent) p.childHasFinished()
  }
  
  def execute(work:ObsoletableAndInterruptableFuture[CommandResult])(implicit executor: ExecutionContext) : CommandResult = {
    try {
      addNotifyOfCancel(work.future.cancel)
      var dataChanged = false
      def dataChangeFN() = { dataChanged=true; work.future.cancel(); this.cancel() }
      work.addChangeListener(dataChangeFN)
      val work2 = work.recover{
        case f:CommandError => CommandResult.failure(f.description)
        case _ if dataChanged => CommandResult.failure("Data changed during computation")
      }
      Await.result(work2.future.future, scala.concurrent.duration.Duration.Inf)
    } finally {
      work.dispose()
    }
  }
}

/**
 * Used for arguments to functions. The functions can then make their own subtask, giving it title and expected work.
 * So most functions that take a ProgressMonitor should actually take a ProgressMonitorPortion and then start with a line
 * like <pre>val progressMonitor = progressMonitorSource.get("This function",Some(6.0))</pre>
 */
class ProgressMonitorSource private[command](ui:Option[ProgressMonitorUI],parent:Option[ProgressMonitor]) {
  def get(name:String,expectedWork:Option[Double],workUnits:Option[String]=None) : ProgressMonitor = {
    val res = new ProgressMonitor(ui,expectedWork,parent)
    for (p<-parent) p.child = Some(res)
    res
  }
}

object ProgressMonitorSource {
  val dummy = new ProgressMonitorSource(None,None)
  def apply(ui:ProgressMonitorUI) = new ProgressMonitorSource(Some(ui),None)
}

class CancelledThrowable extends Throwable

object ProgressMonitor {
  val dummy : ProgressMonitor = new ProgressMonitor(None,None,None)
  def apply(ui:ProgressMonitorUI) = new ProgressMonitor(Some(ui),Some(1.0),None)
}