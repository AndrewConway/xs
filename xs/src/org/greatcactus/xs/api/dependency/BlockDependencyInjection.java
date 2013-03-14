/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.dependency;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Extra annotation to put on an @XSSerializable field to tell it not to propagate dependency injection elements of a particular
 * type from being fed forward to its children.
 * 
 * Normally system dependencies and results of @PropagageToChildren functions will be passed transitively to all subclasses.
 * This lets a class specify a set of classes that will be allowed to reach this container, but not propagate any further.
 * A common reason for doing this would be if you have a class X that contains (amongst other things) instances of the same class.
 * X produce some dependency D that gets propagated to its children. However, a child X may want to make its own D that overrides
 * the parent D. In this case, X could mark D as blocked, and then make the function that produces the new D be marked as
 * @PropagageToChildren (but not @DependencyProvider as well as you would then end up with two Ds available for X)
 * 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface BlockDependencyInjection {
	Class<?>[] value();
}
