/**
 * Copyright Andrew Conway 2008-2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Represent a field in a class for which the entries should be editable in a table.
 * This generally only makes sense if this field is an array or collection, although
 * you could always have exactly one row.
 * @see {@link IndividuallyEditable}
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface TableEditable {
}
