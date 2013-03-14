/**
 * Copyright Andrew Conway 2008-2013. All rights reserved.
 */
package org.greatcactus.xs.api.errors;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * If this field is null or None (or blank, if a string), then that is an error. If the field is a collection, then it produces an error if any elements are blank. See @ErrorIfEmptyCollection for how to produce an error if there is a collection with no elements. 
 * For Options, this annotation is preferred over @ErrorIfEmptyCollection and will produce an error with either None or Some("").
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface ErrorIfBlank {
	/** Is it an error, warning or info? */
	String severity() default "ERROR";
}
