/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.api.dependency
import scala.language.existentials

/**
 * Sometimes information from outside the object being edited is needed - for instance, the contents of an external file.
 * This can be handled using the dependency injection framework by having a function marked @DependencyInjection return
 * an instance of this class. It will then be fed to the (plugged in) external dependency evaluator, which will produce
 * a result of the given class, which will then be treated as if it were the real result of the function.
 * 
 * Furthermore, it can be dynamic - depending upon the (plugged in) external dependency evaluator, the result of this
 * may change which can precipitate a wholescale recomputation of the things that depend upon the produced value 
 */
case class ExternalDependency(val arg:AnyRef,val result:Class[_]) {

}