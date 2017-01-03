/**
 * Copyright 2013-2014 Andrew Conway. All rights reserved.
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
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import java.util.concurrent.ExecutionException

/**
 * Make a future that can be interrupted. Generally behaves like a future, except
 *   * If cancel is called before it starts, then the code will not be executed (it will get completed with an InterruptedException)
 *   * If cancel is called while it is running, then the running code will get an interrupt (of course it is up to said code to do something sensible about such an interrupt).
 *      * If the code finishes without checking, then the interruptible future will check it itself and if interrupted it will get completed with an InterruptedException
 *   * If cancel is called after it has complete, then nothing happens
 *   
 * Note that unlike normal futures, an Interrupted exception completes the future (as a failure).
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
   
  /**
   * Like Future.flatmap, except with cancellation.
   * Note that if :
   *   f3 = f1.flatMap{_ =>f2}
   * where f1 and f2 are both InterruptableFutures (as is f3), then cancelling f3 will cancel f1, but will not cancel f2 unless the function has completed, which will not happen until after f1 is completed.
   * This can lead to unexpected lack of cancellation. 
   */
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
  def onComplete[U](func: Try[T] => U)(implicit executor: ExecutionContext) {
    future.onComplete(func)
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
  def isCompleted = future.isCompleted

}

trait Interruptable {
  private[this] var cancelled = false;
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
        
        for (f<-cancelFunctions) f()
        cancelFunctions.clear()
      }
    }
  }

  def isCancelled = cancelled
  
}

class InterruptablePromise[T] extends Interruptable { ipthis =>
  
  onCancel( () => {if (executingThread!=null) executingThread.interrupt()} )
  
  val promise = concurrent.Promise[T]
  private[this] var executingThread : Thread = null // is not null iff the computation is currently ongoing. Border cases are synchronized.
  
  def executeInAnInterruptableManner[S](code: => S) : S = {
      ipthis.synchronized {
        if (isCancelled) throw new InterruptedException
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
          case e : ExecutionException if e.getCause.isInstanceOf[InterruptedException] => Failure(e.getCause)
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

    /** Create an interruptable future from an exception, computed now rather than later. Slightly more efficient than using future above, but otherwise the same */
  def eagerFailure[T](t: Throwable) : InterruptableFuture[T] = {
    val p = new InterruptablePromise[T]
    p.failure(t)
    p.future
  }
  

  
  /** Analog of Future.sequence. Transforms a `TraversableOnce[InterruptableFuture[A]]` into a `InterruptableFuture[TraversableOnce[A]]`. */
  def sequence[A, M[_] <: TraversableOnce[_]](in: M[InterruptableFuture[A]])(implicit cbf: CanBuildFrom[M[InterruptableFuture[A]], A, M[A]], executor: ExecutionContext): InterruptableFuture[M[A]] = {
    in.foldLeft(eager(cbf(in))) {
      (fr, fa) => for (r <- fr; a <- fa.asInstanceOf[InterruptableFuture[A]]) yield (r += a)
    } map (_.result)
  }

  def apply[T](future:Future[T]) : InterruptableFuture[T] = new SimpleInterruptableFuture(future)

}

class SimpleInterruptableFuture[T](override val future:Future[T]) extends InterruptableFuture[T] with Interruptable 
  

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
  def change() {
    //println("ChangeHandle "+this+" change()")
    val tonotify : List[()=>Unit] = synchronized { 
      if (!hadChange) {
        hadChange = true
        val res = changeListeners
        changeListeners=Nil
        res
      } else Nil
    }
    for (l<-tonotify) l()
  }
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




/**
 * Helpful utility - maintains a list of things that care about some change. Like ChangeHandleBuffer, except multiple use.
 * Useful when you have some global resource (e.g. a file system file) that may change multiple times. Each time
 * it changes, call callAndClear(). This will call change() on all the obsolete handles previously associated with this
 * list.
 */
class OnObsoleteList(val name:String=null) {
  @volatile private[this] var handles : Set[ChangeHandleForObsoleteList] = Set.empty
  
  private[this] var callbackOnEmpty : Set[()=>Unit] = Set.empty // for functions that want to know when something has become empty.
  private[this] var callbackOnNonEmpty : Set[()=>Unit] = Set.empty // for functions that want to know when something has become non-empty.
  
  def remove(h:ChangeHandleForObsoleteList) { 
    val gotToZero = synchronized { handles-=h; handles.isEmpty }
    if (gotToZero) for (h<-callbackOnEmpty) h()
  }
  def add(h:ChangeHandleForObsoleteList) { 
    val wasEmpty = synchronized { val wasEmpty = handles.isEmpty; handles+=h; wasEmpty }
    if (wasEmpty) for (h<-callbackOnNonEmpty) h()
  }

  
  def removeCallbackOnEmpty(h:()=>Unit) { synchronized { callbackOnEmpty-=h }}
  /** add a function that will be called whenever the handle count gets down to zero. */
  def addCallbackOnEmpty(h:()=>Unit) { synchronized { callbackOnEmpty+=h }}

  def removeCallbackOnNonEmpty(h:()=>Unit) { synchronized { callbackOnNonEmpty-=h }}
  /** add a function that will be called whenever the handle count gets down to zero. */
  def addCallbackOnNonEmpty(h:()=>Unit) { synchronized { callbackOnNonEmpty+=h }}

  def callAndClear() {
    val backupHandles = synchronized {
      //println("Clearing handles"+handles.mkString(","))
      val backupHandles = handles
      handles=Set.empty
      backupHandles
    }
    for (h<-backupHandles) h.change()
  }

  /** Like callAndClear, except doesn't actually do the work of calling. Instead returns a set of work to do. This can be done in a separate thread if desired. Returns None in the common case of no work to do. This can be better than just calling callAndClear in a separate thread as fewer runnables will be scheduled */
  def callAndClearReturningWork() : Option[()=>Unit] = {
    val backupHandles = synchronized {
      //println("Clearing handles"+handles.mkString(","))
      val backupHandles = handles
      handles=Set.empty
      backupHandles
    }
    if (backupHandles.isEmpty) None else Some({()=>for (h<-backupHandles) h.change()})
  }
  
  /** For status / debugging */
  def length : Int = handles.size
  def status : String = length.toString+"\n"+handles.mkString("\n")
  def shortStatus : String = if (length==0) "" else length.toString
  def isUsed : Boolean = synchronized { !handles.isEmpty }
  
  def getChangeHandle() : ChangeHandle = new ChangeHandleForObsoleteList(this)
  
}

/**
 * Change handle associated with an OnObsoleteList
 * 
 */

class ChangeHandleForObsoleteList(ool:OnObsoleteList) extends ChangeHandle {
  override def dispose() { 
     //println("ChangeHandleBuffer "+this+" dispose()")
       ool.remove(this) 
  }
  ool.add(this)
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
  def onComplete[U](func: Try[T] => U)(implicit executor: ExecutionContext) {
    future.onComplete(func)
  }
  def cancel() { future.cancel() }

}

object ObsoletableAndInterruptableFuture {
//  def eager[T](value:T) = new ObsoletableAndInterruptableFuture(InterruptableFuture.eager(value),new Obsoletable)
  def eager[T](value:T) = new ObsoletableAndInterruptableFuture(InterruptableFuture.eager(value),Nil)
  def eagerFailure[T](t: Throwable) = new ObsoletableAndInterruptableFuture(InterruptableFuture.eagerFailure(t),Nil)
  
  /** Analog of Future.sequence. Transforms a `TraversableOnce[InterruptableFuture[A]]` into a `InterruptableFuture[TraversableOnce[A]]`. */
  def sequence[A, M[_] <: TraversableOnce[_]](in: M[ObsoletableAndInterruptableFuture[A]])(implicit cbf: CanBuildFrom[M[ObsoletableAndInterruptableFuture[A]], A, M[A]], executor: ExecutionContext): ObsoletableAndInterruptableFuture[M[A]] = {
    in.foldLeft(eager(cbf(in))) {
      (fr, fa) => for (r <- fr; a <- fa.asInstanceOf[ObsoletableAndInterruptableFuture[A]]) yield (r += a)
    } map (_.result)
  }
  
  val alreadyObsoleteHandle = new ChangeHandle{ def dispose() {}}
  alreadyObsoleteHandle.change()

  val alreadyObsolete :ObsoletableAndInterruptableFuture[Nothing]  = new ObsoletableAndInterruptableFuture(InterruptableFuture.eagerFailure(new IsObsoleteException),List(alreadyObsoleteHandle))

  val blank :ObsoletableAndInterruptableFuture[Null]  = new ObsoletableAndInterruptableFuture(InterruptableFuture.eager(null),Nil)
  
  /** Create a future that produces the result, except running said code itself as a future event */
  def future[T](code: => ObsoletableAndInterruptableFuture[T])(implicit executor: ExecutionContext) : ObsoletableAndInterruptableFuture[T] = blank.flatMap{_ => code}
    
}

class IsObsoleteException extends Exception

/**
 * OIFs are not immutable, as they can be cancelled and the obsolete listeners can be disposed.
 * In particular, this means you can't return the same OIF to two different places, which makes
 * caching difficult. This class helps here.
 * 
 * The basic idea: you make an instance of this class out of an OIF. You can then get as many
 * OIFs from it as you like.
 * 
 * Semantics:
 *   (1) A record is kept of the number of listeners using the Obsoleteable portion
 *   (2) cancel called on one of the OIFs will not cancel the original OIF unless it is the only remaining client OIF, in which case the original OIF will be cancelled.
 *   (3) When all client OIFs are disposed, the parent OIF will be disposed [ this can be overridden ]
 *   (4) If a client OIF is requested after the cache is no longer valid (original OIF disposed or cancelled, or gave obsolete message), then an already obsolete result will be produced.
 * 
 */
class ObsoletableAndInterruptableFutureCache[T](original:ObsoletableAndInterruptableFuture[T],implicit val executor:ExecutionContext) {
  private[this] var disposed = false
  private[this] var clients : Set[ObsoletableAndInterruptableFutureCacheClient[T]] = Set.empty
  private[this] var finishedResult : Option[Try[T]] = None
  
  def dispose() {
    synchronized {
      if (!disposed) {
        original.dispose()
        for (c<-clients) c.invalidate()
      }
    }
  }
  
  original.addChangeListener(() => dispose())
  
  private def completed(res:Try[T]) {
    synchronized {
      finishedResult = Some(res)
      for (c<-clients) c.completed(res)
    }
  }
  
  original.future.future.onComplete{completed _}
  
  /** Called when the number of client OIFs has reduced to zero. Can override it if you want to preserve */
  def reachedZeroClients() { original.future.cancel(); dispose() }
  
  def get() : ObsoletableAndInterruptableFuture[T] = {
    synchronized {
      if (disposed) {
        ObsoletableAndInterruptableFuture.alreadyObsolete // invalid result
      } else {
        val client = new ObsoletableAndInterruptableFutureCacheClient(this)
        clients+=client
        for (res<-finishedResult) client.completed(res)
        client.get  
      }
    }
  }
  
  /** Mostly for debugging purposes. */
  def numClients : Int = synchronized { clients.size}
  
  def childDispose(client:ObsoletableAndInterruptableFutureCacheClient[T]) {
    synchronized {
      clients-=client
      if (clients.isEmpty) reachedZeroClients()
    }
  }
  
  def childCancel(client:ObsoletableAndInterruptableFutureCacheClient[T]) {
    synchronized {
      clients-=client
      if (clients.isEmpty) reachedZeroClients()
    }
  }
  
}

private class ObsoletableAndInterruptableFutureCacheClient[T](cache:ObsoletableAndInterruptableFutureCache[T]) { clientthis=>
  
  val obsolete = new ConventionalChangeHandle
  val promise = new InterruptablePromise[T]
  val future = promise.future
  
  obsolete.addDisposalFunction{()=> cache.childDispose(clientthis) }
  future.onCancel{() => cache.childCancel(clientthis)}
  
  def dispose() { obsolete.dispose() }
  def invalidate() { obsolete.change() }
  
  def get : ObsoletableAndInterruptableFuture[T] = new ObsoletableAndInterruptableFuture(future,List(obsolete))
  // extends ConventionalChangeHandle
  
  def completed(res:Try[T]) { promise.complete(res) }
  
}
