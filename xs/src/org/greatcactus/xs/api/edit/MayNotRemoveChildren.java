/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * If on an {@link IndividuallyEditable} or {@link TableEditable} field, then the user may not delete element from this field.
 * If on a class, then an  {@link IndividuallyEditable} or {@link TableEditable} field may not delete members of this class.
 * 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD,ElementType.TYPE})
public @interface MayNotRemoveChildren {
}
