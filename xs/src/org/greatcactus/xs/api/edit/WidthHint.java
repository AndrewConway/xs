/**
 * Copyright Andrew Conway 2012. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * A hint for the relative width of this field.
 * 
 * see {@link StringEditable}
 * 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD,ElementType.TYPE})
public @interface WidthHint {
	/** Approximate number of typical characters */
	int value() default 0;
}
