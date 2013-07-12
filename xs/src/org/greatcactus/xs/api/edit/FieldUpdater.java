/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * When an edit changes a field, usually a new parent object is created using the constructor. 
 * Occasionally you will want to have different behaviour - such as if there are multiple interacting
 * fields, and editing one may change the others. 
 * 
 * To do this, make a function that takes one argument - the new value of the field being changed,
 * and which returns an instance of the parent object. Then give it this annotation with the name
 * of the field it applies to as the argument.
 * 
 * It is an error to have two @FieldUpdater functions in the same class for the same field.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface FieldUpdater {
	/** If blank, then refers to the whole class; otherwise just to a particular field. */
	String value() default "";

}
