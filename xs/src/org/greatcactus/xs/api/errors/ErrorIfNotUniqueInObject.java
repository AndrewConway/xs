/**
 * Copyright Andrew Conway 2008-2013. All rights reserved.
 */
package org.greatcactus.xs.api.errors;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * If this field is not unique amongst other elements in the class this is a field of that have the same key, then that is an error.
 * This is not very useful for a field that is not a collection or doesn't have the "key" element of the annotation added as there is by definition at most one elements.
 * If the field is a collection, it checks that each element of the collection is unique.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface ErrorIfNotUniqueInObject {
	/** Is it an error, warning or info? */
	String severity() default "ERROR";
	/** Normally uniqueness will be checked against just elements of this particular field. If the key is set, then uniqueness will be against everything with the same key. */
	String key() default "";
}
