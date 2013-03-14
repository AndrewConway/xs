/**
 * Copyright Andrew Conway 2008-2013. All rights reserved.
 */
package org.greatcactus.xs.api.errors;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * If this field is not unique in the object being edited, then that is an error.
 * 
 * The implementation is reasonably efficient (linear in data structure size), but this is potentially expensive.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface ErrorIfNotGloballyUnique {
	/** Is it an error, warning or info? */
	String severity() default "ERROR";
	/** Normally uniqueness will be checked against just elements of this particular field. If the key is set, then uniqueness will be against everything with the same key. */
	String key() default "";
}
