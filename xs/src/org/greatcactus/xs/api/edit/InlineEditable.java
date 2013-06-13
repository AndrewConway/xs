/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Represent a field in a class which is XS editable that should be included inline as
 * if its fields were members of this class.
 * 
 * The field is really a collection of XS editable things.
 *  * Option is a 0 or 1 length collection
 *  * Plain direct member is a 1 length collection [or 0 if null]
 * If the collection contains n elements, then n copies of all the fields
 * in the subclasses will be displayed. Delete will be present unless the 
 * field is a plain direct member.
 * 
 * @see {@link TableEditable} {@link InlineEditable}
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface InlineEditable {
}
