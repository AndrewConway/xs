/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.util

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Success
import scala.util.control.NonFatal
import scala.util.Failure
import java.lang.InterruptedException
import scala.util.Try
import scala.collection.mutable.ListBuffer
import org.greatcactus.xs.api.command.ProgressMonitor

/**
 * Make a future that can be interrupted. Generally behaves like a future, except
 *   * If cancel is called before it starts, then the code will not be executed (it will get completed with an InterruptedException)
 *   * If cancel is called while it is running, then the running code will get an interrupt (of course it is up to said code to do something sensible about such an interrupt).
 *      * If the code finishes without checking, then the interruptible future will check it itself and if interrupted it will get completed with an InterruptedException
 *   * If cancel is called after it has complete, then nothing happens
 *   
 * Note that unlike normal futures, an Interrupted exception is completes the future (as a failure).
 * 
 * For the monadic combinators, cancel has the semantics that cancelling the newly created one will cancel the older ones, but not vice versa.
 */
abstract class InterruptableFuture[+T] {
   val future : Future[T]
   def cancel()
   
   def onCancel(f:()=>Unit)
   
   def foreach[U](f: T => U)(implicit executor: ExecutionContext): Unit = future.onComplete {
     case Success(r) => f(r)
     case _  => // do nothing
   }(executor)
   
   def map[S](f: T => S)(implicit executor: ExecutionContext): InterruptableFuture[S] = { // transform(f, identity)
      val p = new InterruptablePromise[S]
      p.onCancel(()=> cancel())
      future.onComplete {
        case Success(r) => p.completeCode{f(r)}
        case f: Failure[_] => p complete f.asInstanceOf[Failure[S]]
      }(executor)
      p.future
   }
   
  def flatMap[S](function: T => InterruptableFuture[S])(implicit executor: ExecutionContext): InterruptableFuture[S] = {
    val p = new InterruptablePromise[S]
    p.onCancel(()=> cancel())
    future.onComplete {
      case f: Failure[_] =>
        p complete f.asInstanceOf[Failure[S]]
      case Success(v) =>
        try {
          val ifv = p.executeInAnInterruptableManner(function(v)) 
          p.onCancel(() => ifv.cancel())
          ifv.future.onComplete({
            case f: Failure[_] => p complete f.asInstanceOf[Failure[S]]
            case Success(v) => p success v
          })(executor)
        } catch {
          case e : InterruptedException => p failure e
          case NonFatal(t) => p failure t
        }
    }(executor)
    p.future
  }
  def filter(pred: T => Boolean)(implicit executor: ExecutionContext): InterruptableFuture[T] = {
    val p = new InterruptablePromise[T]
    p.onCancel(()=> cancel())
    future.onComplete {
      case f: Failure[_] => p complete f.asInstanceOf[Failure[T]]
      case Success(v) => p.completeCode{
          if (pred(v)) v else throw new NoSuchElementException("Future.filter predicate is not satisfied")
        }
    }(executor)

    p.future
  }
  final def withFilter(p: T => Boolean)(implicit executor: ExecutionContext): InterruptableFuture[T] = filter(p)(executor)

  def recover[U >: T](pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): InterruptableFuture[U] = {
    val p = new InterruptablePromise[U]
    p.onCancel(()=> cancel())

    future.onComplete { case tr => p.complete(tr recover pf) }(executor)

    p.future
  }
  
  /** Note that the code executed will not check for interruptions */
  def andThen[U](pf: PartialFunction[Try[T], U])(implicit executor: ExecutionContext): InterruptableFuture[T] = {
    val p = new InterruptablePromise[T]
    p.onCancel(()=> cancel())

    future.onComplete {
      case r => try { if (pf isDefinedAt r) pf(r)} finally { p complete r }
    }(executor)

    p.future
  }

}

class InterruptablePromise[T] { ipthis =>
  val promise = concurrent.promise[T]
  private[this] var cancelled = false;
  private[this] var executingThread : Thread = null // is not null iff the computation is currently ongoing. Border cases are synchronized.
  private[this] val cancelFunctions = new ListBuffer[() => Unit]
  
  def onCancel(f: ()=>Unit) {
    synchronized {
      if (cancelled) f()
      else cancelFunctions+=f
    }
  }
  
  def cancel() {
    //println("Attempting to cancel future")
    synchronized {
      if (!cancelled) {
        cancelled = true
        if (executingThread!=null) executingThread.interrupt()
        for (f<-cancelFunctions) f()
        cancelFunctions.clear()
      }
    }
  }
  
  def executeInAnInterruptableManner[S](code: => S) : S = {
      ipthis.synchronized {
        if (cancelled) throw new InterruptedException
        Thread.interrupted() // if the thread is already interrupted, that is by something else.
        executingThread = Thread.currentThread()      
      }
      val res = code
      synchronized {
        executingThread = null
        if (Thread.interrupted()) throw new InterruptedException
      }
      res    
  }
  def completeCode(code: =>T) {
    promise complete {
        try Success(executeInAnInterruptableManner(code)) catch {
          case e : InterruptedException => Failure(e)
          case NonFatal(e) => Failure(e)
          case other : Throwable => other.printStackTrace(); throw other
        }
      }
  }
  
  def complete(result: Try[T]) = try {
    promise.complete(result)
  } catch {
    case NonFatal(t) => promise failure t
  }
  def success(v: T) = promise.complete(Success(v))
  def failure(t: Throwable) = promise.failure(t)
  
  val future : InterruptableFuture[T] = new InterruptableFuture[T] {
    def cancel() { ipthis.cancel() }
    def onCancel(f: ()=>Unit) { ipthis.onCancel(f) }
    val future = promise.future
    
    
  }
  
}


object InterruptableFuture {
  def future[T](code: =>T)(implicit ex: ExecutionContext) : InterruptableFuture[T] = {
    val p = new InterruptablePromise[T]
    ex.execute(new Runnable() {
      override def run() { p.completeCode(code) }
    })
    p.future
  } 
  
  /** Create an interruptable future from a simple value, computed now rather than later. Slightly more efficient than using future above, but otherwise the same */
  def eager[T](value:T) : InterruptableFuture[T] = {
    val p = new InterruptablePromise[T]
    p.success(value)
    p.future
  }
}


/**
 * When a function returns a result that may become obsolete at some future time, it can also return a ChangeHandle. This allows
 * keeping track of whether it changes.
 * 
 * When something gets a ChangeHandle as response, it MUST make sure that the dispose() function is eventually called, otherwise
 * there may be memory leaks. 
 */

trait ChangeHandle {
  private var changeListeners : List[()=>Unit] = Nil
  private var hadChange = false

  /** Call to add a function that will be called (once) when there is some change. This handle will then be obsolete. */
  def addChangeListener(f: () => Unit) { 
    synchronized { 
      if (hadChange) f()
      else changeListeners = f::changeListeners
    }
  }
  /** Call when you no longer care about possible changes. */
  def dispose()
  
  /** Call when a change actually happens */
  def change() { synchronized { 
    //println("ChangeHandle "+this+" change()")
    if (!hadChange) {
      hadChange = true
      for (l<-changeListeners) l()
      changeListeners=Nil
    }
  }}
}

/**
 * A change handle that can easily retrofit into the explicit change listener/disposal function world.
 * use:
 *   Create one
 *   call addDisposalFunction with the function that should be called when the handle is no longer needed.
 *   use handle.change() as the function that should be called when the thing the handle represents becomes obsolete
 */
class ConventionalChangeHandle extends ChangeHandle {
  var handles : List[()=>Unit] = Nil
  var disposed = false

  def addDisposalFunction(h: ()=>Unit) {
    synchronized {
      if (disposed) h()
      else {
        handles=h::handles
      }
    }
  }
  override def dispose() { 
    //println("ChangeHandleBuffer "+this+" dispose()")
    synchronized {
      if (!disposed) {
        disposed=true
        for (h<-handles) h()
        handles = Nil    
      }
    }
  }
  
}

/**
 * A change handle that represents a set of other change handles, which are not necessarily all known yet.
 * A dispose of this will dispose all the child handles; a change on one of the child handles will call change on this.
 */
class ChangeHandleBuffer extends ChangeHandle {
  var handles : List[ChangeHandle] = Nil
  var disposed = false

  def add(h:ChangeHandle) {
    synchronized {
      if (disposed) h.dispose()
      else {
        handles=h::handles
        h.addChangeListener(change)
      }
    }
  }
  override def dispose() { 
    //println("ChangeHandleBuffer "+this+" dispose()")
    synchronized {
      if (!disposed) {
        disposed=true
        for (h<-handles) h.dispose()
        handles = Nil    
      }
    }
  }
}


class ObsoletableAndInterruptableFuture[+T](val future:InterruptableFuture[T],val changes:List[ChangeHandle]) {
    
  def map[S](f: T => S)(implicit executor: ExecutionContext): ObsoletableAndInterruptableFuture[S] = { // transform(f, identity)
     new ObsoletableAndInterruptableFuture(future.map(f)(executor),changes)
  }
  
  def flatMap[S](f: T => ObsoletableAndInterruptableFuture[S])(implicit executor: ExecutionContext): ObsoletableAndInterruptableFuture[S] = {
    val buffer = new ChangeHandleBuffer
    def modf(t:T) : InterruptableFuture[S] = {
      val oif = f(t)
      for (c<-oif.changes) buffer.add(c)
      oif.future
    }
    val nf = new ObsoletableAndInterruptableFuture(future.flatMap(modf)(executor),buffer::changes)
    nf
  }
   
  def filter(pred: T => Boolean)(implicit executor: ExecutionContext): ObsoletableAndInterruptableFuture[T] = {
    new ObsoletableAndInterruptableFuture(future.filter(pred)(executor),changes)
  }
  final def withFilter(p: T => Boolean)(implicit executor: ExecutionContext): ObsoletableAndInterruptableFuture[T] = filter(p)(executor)

  def recover[U >: T](pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): ObsoletableAndInterruptableFuture[U] = {
    new ObsoletableAndInterruptableFuture(future.recover(pf)(executor),changes)
  }
  
  def andThen[U](pf: PartialFunction[Try[T], U])(implicit executor: ExecutionContext): ObsoletableAndInterruptableFuture[T] = {
    new ObsoletableAndInterruptableFuture(future.andThen(pf)(executor),changes)
  }
  
  def asProgress(progressMonitor:ProgressMonitor,amount:Double)(implicit executor: ExecutionContext) : ObsoletableAndInterruptableFuture[T] = andThen{case _ => progressMonitor.doUnitWork(amount)}

  def dispose() {
    for (c<-changes) c.dispose()
  }
  def addChangeListener(onExternalChange: ()=>Unit) {
    for (c<-changes) c.addChangeListener(onExternalChange)
  }  
  /** dispose, after it has completed */
  def disposeOnComplete()(implicit executor: ExecutionContext) {
    future.future.onComplete{case _ => this.dispose() } 
  }

}

object ObsoletableAndInterruptableFuture {
//  def eager[T](value:T) = new ObsoletableAndInterruptableFuture(InterruptableFuture.eager(value),new Obsoletable)
  def eager[T](value:T) = new ObsoletableAndInterruptableFuture(InterruptableFuture.eager(value),Nil)
}




