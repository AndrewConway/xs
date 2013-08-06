/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.command;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to say that a method returns a collection of EditCommandDescription objects, which will be used
 * as extra commands available to the user that, when activated, produce a new instance of the class of which
 * this is a member, which will then replace that class. Multiple (or 0) objects may be returned for
 * multiple commands.
 * 
 * This method can take the usual dependency injection parameters.
 * 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface XSEditCommands {
	
}
