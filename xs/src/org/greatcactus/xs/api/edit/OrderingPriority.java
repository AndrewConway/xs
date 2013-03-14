/**
 * Copyright Andrew Conway 2010-2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The order that fields are presented on the screen is generally the order that they are encountered
 * in the .class file. You can make this explicit by entering a value for this. Higher values go later.
 * 
 * see {@link EditSection}
 * 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD,ElementType.TYPE})
public @interface OrderingPriority {
	/** priority of this field. */
	int value() default 0;
}
