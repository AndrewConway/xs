/**
 * Copyright Andrew Conway 2008-2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Represent a field in a class for which the entries should be editable by adding
 * or deleting or editing individual ones. Generally works for entries too complex
 * to be edited in a table. Generally will be edited as part of a tree.
 * 
 * This generally only makes sense if this field is an array or collection, although
 * a normal field could be null or not, or even just a bunch of stuff to be edited
 * separately. 
 * @see {@link TableEditable} {@link ExpandOnFirstDisplay}
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface IndividuallyEditable {
}
