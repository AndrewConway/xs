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
 * This annotation is put on a function in the class, and affects a named field or section. It should return a boolean. (or a future thereof)
 * 
 * Fields are visible by default. If this is present, then they are invisible unless true is returned. This means if a future is returned, the field will be invisible until the future is resolved.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface VisibilityController {
	/** The field controlled by this function. */
	String value();

}
