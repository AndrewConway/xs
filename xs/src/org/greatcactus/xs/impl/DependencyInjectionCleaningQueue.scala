/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.impl

import org.greatcactus.xs.frontend.XSTreeNode
import java.util.concurrent.Semaphore

/**
 * When to recompute dependency injection is a difficult decision. Obvious choices are
 * <ul>
 *   <li>Whenever it is made dirty (that is, needs recomputation). Advantages: quick response, simple implementation. Disadvantages: If the server comes under load, you will end up recomputing things too frequently</li>
 *   <li>A second after it is made dirty. Advantages: little wasted server time. Disadvantages: Slow perceived response
 * </ul>
 *
 * <p>
 * The actual route taken is somewhere between those two extremes.
 * When a node is made dirty, it is added onto a priority queue in this class.
 * When whatever thread made it dirty has finished doing whatever makes it dirty, it calls cleanup on this class.
 * Cleanup sees if some other thread is already working on cleaning up. If so, it returns immediately. Otherwise it becomes the cleanup thread (until the queue is empty).
 * </p>
 * 
 * <p>
 * The cleanup thread needs to do parents before children. This could be managed by just using a priority queue based on depth. This is what it does. 
 * </p>
 * 
 * <p>Some premature optimization I was tempted to do:
 * This is pretty close to
 * what does in fact occur. The (minor) problem with this approach is that if you have a tree structure with many branches, and do something that modifies
 * a dependency that propagates to children starting from the root, then that will propagate all the way down and cause a complete covering of the tree. [ Note: the
 * thing that causes the dependency problem will initiate a clean up. ] Then the clean-up thread would process one child of the root. It would create new
 * dependencies for each of its children, causing them to need cleaning up. It would not actually do any of those children now, but would rather
 * go back to the queue and get a new first child of the root node. This will result in a breadth first traversal of the tree. This is slightly 
 * inferior to depth first search (makes queue longer, and is likely to be worse cache-wise). So we have an optimization that says that, after cleaning
 * a node (which it knows to not have any parent in the queue), it will consider cleaning its children rather than putting them into the queue. It will
 * do this if no new nodes have been added to the queue since it started work. It also reduces load in the queue which may make the common case of an irrelevent 
 * change propagating through many nodes which don't use dependency injection to be significantly faster. It may also be a waste of effort. In fact it almost
 * certainly is. So It is not done. Sorry for wasting your time. I left this comment in in case the optimization is desired later.
 * </p>
 */
class DependencyInjectionCleaningQueue {

  private val nodesNeedingCleaning = new scala.collection.mutable.PriorityQueue[XSTreeNode]()(new Ordering[XSTreeNode]{ override def compare(n1:XSTreeNode,n2:XSTreeNode) = n2.depth-n1.depth}) // want smaller depth first, so unusual ordering.
  
  def add(node:XSTreeNode) {
    synchronized {
      nodesNeedingCleaning+=node  
      //println("nodesNeedingCleaning length = "+nodesNeedingCleaning.length)
    }
  }
  
  private[this] val someThreadIsAlreadyCleaning = new Semaphore(1)
    
  def cleanWaitingUntilAllClean() {
    someThreadIsAlreadyCleaning.acquire()
    doRealWork()
  }
  
  def cleanReturningInstantlyIfSomeOtherThreadIsAlreadyCleaning() {
    if (someThreadIsAlreadyCleaning.tryAcquire()) doRealWork()
  }
  
  def isEmpty = synchronized { nodesNeedingCleaning.isEmpty }
  
  private[this] def doRealWork() {
    try {
      while (!isEmpty) {
        //println("Cleaning : nodesNeedingCleaning length = "+nodesNeedingCleaning.length)
        val node = synchronized { nodesNeedingCleaning.dequeue() }
        node.cleanDependencies()
      }
    } finally {
      someThreadIsAlreadyCleaning.release()
    }
    if (!isEmpty) cleanWaitingUntilAllClean() // deal with the (unusual) race condition where some other thread both adds a node to the queue, and calls cleanReturningInstantlyIfSomeOtherThreadIsAlreadyCleaning(), in between the empty check and the release of the semaphore.
  }
   
}