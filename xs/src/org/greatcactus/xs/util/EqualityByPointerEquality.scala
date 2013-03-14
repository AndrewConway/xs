/**
 * Copyright Andrew Conway 2012. All rights reserved.
 */
package org.greatcactus.xs.util

/**
 * A holder for an object that tests to see if another object is equals using pointer equality (and using pointer hashcode)
 *
 */
class EqualityByPointerEquality[+T <: AnyRef](val contents:T) {

  override def equals(other:Any) : Boolean = other match {
    case o:EqualityByPointerEquality[_] => o.contents eq contents
    case _ => false
  }
  
  override def hashCode : Int = contents.##
}

object EqualityByPointerEquality {
  def apply[T <: AnyRef](contents:T) = new EqualityByPointerEquality(contents)
  
  def listsEqualByPointerEquality[T <: AnyRef](l1:Seq[T],l2:Seq[T]) : Boolean = {
    if (l1.length!=l2.length) false
    //else List.forall2(l1,l2){_ eq _}
    else (l1,l2).zipped.forall{_ eq _}
  }
}