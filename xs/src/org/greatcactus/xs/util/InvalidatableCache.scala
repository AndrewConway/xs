/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.util

/**
 * Store something that can be made invalid externally.
 */
class InvalidatableCache[T](compute : => T) {
   private[this] var cache : Option[T] = None
   
   def invalidate() { synchronized {cache=None} }
   def get : T = {
     synchronized {
       if (!cache.isDefined) cache=Some(compute)
       cache.get
     }
   }
}