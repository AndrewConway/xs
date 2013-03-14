/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.frontend

/**
 * Generate a sequence of unique IDs, unique amongst this instance of the class (rather than globally unique, or even unique for this JVM)
 *
 */
class UniqueIDSource {

  private var count = 0L
  
  def newID() : Long = {
    count+=1
    count
  }
}