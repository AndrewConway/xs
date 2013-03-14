/**
 * Copyright Andrew Conway 2008-2013. All rights reserved.
 */
package org.greatcactus.xs.api.errors;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * If this field is not unique amongst other elements in the children of the parent of the class this is a field of (that have the same key), then that is an error.
 * 
 * That is, if class X contains a list of class Y, and Y contains a field with this annotation, then the uniqueness will be checked across all Y's in X.
 * If the field is a collection, it checks that each element of the collection is unique.
 * 
 * The implementation is reasonably efficient (linear in parent data structure size), but this is potentially expensive.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface ErrorIfNotUniqueInParent {
	/** Is it an error, warning or info? */
	String severity() default "ERROR";
	/** Normally uniqueness will be checked against just elements of this particular field. If the key is set, then uniqueness will be against everything with the same key. */
	String key() default "";
}
