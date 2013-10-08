/**
 * Copyright Andrew Conway 2008-13. All rights reserved.
 */
package org.greatcactus.xs.api.serialization;

import java.lang.annotation.*;

/**
 * <p>This tag indicates an obsolete field for the class. Such fields should be ignored quietly if seen in a file
 * being deserialized. Currently only implemented for XML attributes.</p>
 * 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface XSIgnorableNames {
	/** The obsolete name(s)  */
	String[] value(); 
}
