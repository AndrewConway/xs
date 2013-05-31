/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.display;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Similar to {{{LabelProvider}}}, but provide a tooltip. The result can be of the same type. 
 * 
 * If no argument is given to the annotation, then this refers to the whole class (and is currently not displayed). 
 * If an argument is given, then
 * it should be the name of a field, and the label will then apply to that field.
 * 
 * It is an error to have two @TooltipProvider functions in the same class that provide labels for the same thing.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface TooltipProvider {
	/** If blank, then refers to the whole class; otherwise just to a particular field. */
	String value() default "";

}
