/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.api.serialization;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Wraps this field in an extra tag.
 * 
 * Fields are generally direct subtags of the parent tag, with the tag text denoting the class. 
 * Polymorphic fields have their type resolved by this name.
 * 
 * If two fields have the same class, then XSName is used to override the name and let
 * the fields be distinguished. But this does not work with polymorphic fields. In that
 * case, you need this instead of XSName (it doesn't make much sense to have both).
 * 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface XSWrapper {
	/** The name of the tag. Blank (or null) means the name of the field */
	String value() default "";
}
