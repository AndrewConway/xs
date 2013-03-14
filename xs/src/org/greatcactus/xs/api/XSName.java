/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.api;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Fields generally have a name equal to their name.
 * 
 * This lets you use a different name given by the name of the argument.
 * If you are trying to distinguish multiple overlapping polymorphic types - instead use XSWrapper.
 * 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface XSName {
	/** The new name of the field */
	String value() default "";
}
