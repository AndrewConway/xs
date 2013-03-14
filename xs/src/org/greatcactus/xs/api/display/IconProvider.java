/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.display;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * While editing in an XS tree, classes that form tree nodes are shown with an optional icon and label; 
 * the icon usually comes from @XSIcon, but can be produced by another function using this
 * annotation.
 * <ul>
 *   <li>null, in which case @XSIcon is used</li>
 *   <li>A string, in which case it is considered as a logical name of an icon (as if the string in @XSIcon).</li>
 *   <li>An Icon class or ConcreteIcon trait.</li>
 * </ul>
 * 
 * If no argument is given to the annotation, then this refers to the whole class. If an argument is given, then
 * it should be the name of a field, and the label will then apply to that field.
 * 
 * It is an error to have two @IconProvider functions in the same class that provide icons for the same thing.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface IconProvider {
	/** If blank, then refers to the whole class; otherwise just to a particular field. */
	String value() default "";
}
