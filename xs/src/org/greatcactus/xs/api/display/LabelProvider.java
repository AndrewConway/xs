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
 * the label is normally provided by #toString() but can be produced by another function using this
 * annotation. As well as allowing the use of dependency injection, it allows more flexibility in
 * output than just #toString(). The result type must be:
 * <ul>
 *   <li>null, in which case #toString() is used</li>
 *   <li>A string, in which case said string is used.</li>
 *   <li>A Localizable value, in which case it is called with the current locale and the result is used.</li>
 *   <li>An scala.xml.NodeSeq value, in which case it is assumed to be HTML. An HTML capable client will use the HTML, otherwise it will be converted to a string (ignoring all tags)</li>
 *   <li>A LazyLabel, which will not be evaluated if it is not currently displayed.</li>
 *   <li>A RichLabel structure, in which case the HTML field will be used if possible, otherwise the text field.</li>
 * </ul>
 * 
 * If no argument is given to the annotation, then this refers to the whole class. If an argument is given, then
 * it should be the name of a field, and the label will then apply to that field.
 * 
 * It is an error to have two @LabelProvider functions in the same class that provide labels for the same thing.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface LabelProvider {
	/** If blank, then refers to the whole class; otherwise just to a particular field. */
	String value() default "";

}
