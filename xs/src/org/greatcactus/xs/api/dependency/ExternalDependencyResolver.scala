/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.api.dependency

/**
 * External dependencies are resolved to concrete values by an instance of this class instantiated by the client.
 * 
 * The result type of the request should come from the given arguments and the requested type. 
 * Sometimes external references may change with time, which means that the result formerly produced may become obsolete.
 * In that case, the dependency resolver should call the provided obsoleteCallBack function when it becomes
 * obsolete. This should never be called more than once. This means that the client code will have
 * to keep a list of obsoleteness listeners. If the client does so, it should return, in the  ExternalDependencyResult
 * result, a function that XS can call to say that it no longer uses that result, and there is no need
 * to bother tracking the previous callback. [ In the listener paradigm the resolve() function implicitly
 * does an addListener with the onObsoleteCallback function, and then calling the onNoLongerUsed function
 * will do a removeListener. ]   
 *
 */
abstract class ExternalDependencyResolver {
  def resolve(dep:ExternalDependency,onObsoleteCallback: () => Unit) : ExternalDependencyResult
}

class ExternalDependencyResult(val result:AnyRef,val onNoLongerUsed: Option[() => Unit])