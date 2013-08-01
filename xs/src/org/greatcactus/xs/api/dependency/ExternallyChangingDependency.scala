/**
 * Copyright (c) 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.api.dependency

/**
 * Used when a dependency function returns a value which may become incorrect at some future time
 * (e.g. it might be the contents of a file which may be externally changed). 
 * Has a result, together with a function that is called when the result is not used any more.
 * Note that if the results are a collection or array or option, then the subelements will be added (not recursively).
 * 
 * Note that a frequently better alternative to this approach is to use ObsoletableCancellableFuture.
 */
class ExternallyChangingDependency(val actual:AnyRef,val onNoLongerUsed:()=>Unit) 

/** 
 *  A companion to ExternallyChangingDependency. This can be requested as a dependency injection argument.
 *  It will be provided by XS, and is a function that some external event may call to say that the result
 *  of this function is obsolete and should be abandoned.
 */
class OnObsoleteCallback(val function: ()=>Unit)