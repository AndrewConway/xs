/**
 * Copyright Andrew Conway 2008-2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Represent a tag that should be edited as a simple string. This can include enumerations, numbers, or anything with a decent string serialization/deserialization. </p>
 *
 * The run time type options are the same as for {@link XMLSerializeAsString},
 * as is the encoding of arrays and collections.
 *
 * @see {@link XMLSerializeAsString}
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface StringEditable {
	/** If not blank, then a suggested format. These are somewhat client dependent, examples (HTML5) are date, email, tel, time. */
	String knownInterpretation() default "";
}
