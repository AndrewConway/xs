/**
 * Copyright Andrew Conway 2008-2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * If editing of fields is to be divided up into several areas, describe
 * the name of the area that this field should go into. If this annotation will
 * not use, the field will go into a section called "Main".
 * 
 * The annotation can be also be put on the class
 * rather than the field. Field entries will override the class entry.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD,ElementType.TYPE})
public @interface EditSection {
	/** The section to edit in */
	String value() default "General";
}
