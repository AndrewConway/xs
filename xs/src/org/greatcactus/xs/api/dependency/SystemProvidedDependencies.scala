/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.api.dependency

/**
 * A frequent source of dependency injection is as some (named by a class) wrapper around some class that isn't unique enough in its own right to be identifiable.
 * This could be done simply with class Parent[T <: AnyRef](val get:T), but is is good to have equals and hashCode and toString defined. One could use a case
 * class by preceding it with "case", and this is fine, but instead you can do "extends ObjectHolder[T]" at the end. This more clearly defines the purpose
 * of the class. [ It also is probably slightly more efficient implementation wise. ]
 *
 */
abstract class ObjectHolder[T] {
  val get:T // abstract value, should be overridden in user type
  override def hashCode = get.hashCode
  override def equals(other:Any) = other match {
    case h:ObjectHolder[_] => get==h
    case _ => false
  }
  override def toString = get.toString
}

/**
 * Class passed to children as an available dependency, containing a reference to the immediate parent.
 */
class Parent[T <: AnyRef](val get:T) extends ObjectHolder[T]