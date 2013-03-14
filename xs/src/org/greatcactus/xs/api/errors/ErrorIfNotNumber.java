/**
 * Copyright Andrew Conway 2009-2013. All rights reserved.
 */
package org.greatcactus.xs.api.errors;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * If this field is not a number, then that is an error. Blank is not considered an error. (see ErrorIfNull if you want blank to be an error)
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface ErrorIfNotNumber {
	/** Is it an error, warning or info? */
	String severity() default "ERROR";
	boolean integer() default false;
	double min() default Double.NaN;
	double max() default Double.NaN;
}
