/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.util;

import org.junit.Assert._
import org.junit.Test
import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent._
/**
 * Test interruptable futures
 * 
 * Note that some of the assertions here are time dependent, and may fail on a very heavily loaded system.
 *
 */
class ZZZ_InterruptableFuture {

  import concurrent.ExecutionContext.Implicits.global
  
  def now[T](what:InterruptableFuture[T]) : T = Await.result(what.future,0 nanos)
  def soon[T](what:InterruptableFuture[T]) : T = Await.result(what.future,1 second)
  
	@Test
	def testEager() {
	  val e = InterruptableFuture.eager{7}
	  assertEquals(7,now(e))
	  val e2 = InterruptableFuture.future{8}
	  assertEquals(8,soon(e2))
	}

  	@Test
	def testWithDelay() {
	  val e1 = InterruptableFuture.future{Thread.sleep(50); 9}
	  try {
	    now(e1)
	    assertFalse(true) // should throw exception
	  } catch {
	    case t:TimeoutException => // OK
	  }
	  assertEquals(9,soon(e1))
	}

  	@Test
  	def testWithException() {
	  val e1 = InterruptableFuture.future{throw new IllegalArgumentException()}
	  try {
	    soon(e1)
	    assertFalse(true) // should throw exception
	  } catch {
	    case t:IllegalArgumentException => // OK
	  }
  	}
  	
  	def testSmallDelay(expected:Int,future:InterruptableFuture[Int]) {
	  try {
	    now(future)
	    assertFalse(true) // should throw exception
	  } catch {
	    case t:TimeoutException => // OK
	  }
	  assertEquals(expected,soon(future))
  	}
  	def testExceptionSmallDelay(future:InterruptableFuture[_]) {
	  try {
	    now(future)
	    assertFalse(true) // should throw exception
	  } catch {
	    case t:TimeoutException => // OK
	  }
	  try {
	    soon(future)
	    assertFalse(true) // should throw exception
	  } catch {
	    case t:IllegalArgumentException => // OK
	  }
  	}


  	@Test
  	def testWithCombinators() {
	  val t3 = InterruptableFuture.future{Thread.sleep(50); 3}
	  val t4 = InterruptableFuture.future{Thread.sleep(50); 4}
	  val t5 = InterruptableFuture.future{Thread.sleep(50); 5}
	  val t6 = InterruptableFuture.future{Thread.sleep(50); 6}
	  testSmallDelay(6,for (v<-t3) yield v*2)
	  testSmallDelay(9,for (v<-t4;u<-t5) yield v+u)
	  testSmallDelay(10,for (v<-t4;u<-t6) yield v+u)
  	}
  	
  	@Test
  	def testExceptionsWithCombinators() {
	  val t3 = InterruptableFuture.future{Thread.sleep(50); 3}
	  val t4 = InterruptableFuture.future{Thread.sleep(50); 4}
	  val t5 = InterruptableFuture.future{Thread.sleep(50); 5}
	  val t6 = InterruptableFuture.future{Thread.sleep(50); 6}
	  val i1 = InterruptableFuture.future{Thread.sleep(50); throw new IllegalArgumentException}
	  val i2 = InterruptableFuture.future{Thread.sleep(50); throw new IllegalArgumentException}
	  val i3 = InterruptableFuture.future{Thread.sleep(50); throw new IllegalArgumentException}
	  val i4 = InterruptableFuture.future{Thread.sleep(50); throw new IllegalArgumentException}
	  val i5 = InterruptableFuture.future{Thread.sleep(50); throw new IllegalArgumentException}
	  val i6 = InterruptableFuture.future{Thread.sleep(50); throw new IllegalArgumentException}
	  testExceptionSmallDelay(i1)
	  testExceptionSmallDelay(for (v<-i2) yield 8)
	  testExceptionSmallDelay(for (v<-i3;v2<-t3) yield 8)
	  testExceptionSmallDelay(for (v<-t4;v2<-i4) yield 8)
	  testExceptionSmallDelay(for (v<-i5;v2<-i6) yield 8)
  	}
  	
  	@Test
  	def testCancel() {
  	  val e1 = InterruptableFuture.future{Thread.sleep(3000); 3}
  	  try {
	    now(e1)
	    assertFalse(true) // should throw exception
	  } catch {
	    case t:TimeoutException => // OK
	  }
      e1.cancel()
  	  try {
	    soon(e1)
	    assertFalse(true) // should throw exception
	  } catch {
	    case t:InterruptedException => // OK
	    case t:ExecutionException => // OK
	  }
      
  	}


}
