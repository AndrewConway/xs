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

/**
 * Make a future that can be interrupted. Generally behaves like a future, except
 *   * If cancel is called before it starts, then the code will not be executed (it will get completed with an InterruptedException)
 *   * If cancel is called while it is running, then the running code will get an interrupt (of course it is up to said code to do something sensible about such an interrupt).
 *      * If the code finishes without checking, then the interruptible future will check it itself and if interrupted it will get completed with an InterruptedException
 *   * If cancel is called after it has complete, then nothing happens
 *   
 * Note that unlike normal futures, an Interrupted exception is completes the future (as a failure).
 */
abstract class InterruptableFuture[T] {
   val future : Future[T]
   def cancel()
}

class InterruptableFutureImpl[T](code: =>T,ex: ExecutionContext) extends InterruptableFuture[T] {
  private[this] var cancelled = false;
  private[this] var executingThread : Thread = null // is not null iff the computation is currently ongoing. Border cases are synchronized.
  
  def cancel() {
    //println("Attempting to cancel future")
    synchronized {
      if (!cancelled) {
        cancelled = true
        if (executingThread!=null) executingThread.interrupt()      
      }
    }
  }
  
  private[this] def doWork() : T = {
    //println("Starting work on future - cancelled = "+cancelled)
    synchronized {
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
  val promise = concurrent.promise[T]
  
  ex.execute(new Runnable() { // like a normal future, but catch InterruptedException.
    override def run() = {
      promise complete {
        try Success(doWork()) catch {
          case e : InterruptedException => Failure(e)
          case NonFatal(e) => Failure(e) 
        }
      }
    }
  })
  
  val future = promise.future
  
}

object InterruptableFuture {
  def future[T](code: =>T)(implicit ex: ExecutionContext) : InterruptableFuture[T] = new InterruptableFutureImpl(code,ex)
}

