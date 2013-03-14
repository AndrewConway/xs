/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.display;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Control whether a field is visible or not in the details pane
 * 
 * This annotation is put on a function in the class, and affects a named field or section. It should return a boolean.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface VisibilityController {
	/** The field controlled by this function. */
	String value();

}
