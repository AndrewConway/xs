/**
 * Copyright Andrew Conway 2008-13. All rights reserved.
 */
package org.greatcactus.xs.api.serialization;

import java.lang.annotation.*;

/**
 * <p>This tag indicates an obsolete name for the class or field.
 * The name will be used for loading, but not saving.</p>
 * 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD,ElementType.TYPE})
public @interface XSObsoleteName {
	/** The obsolete name(s)  */
	String[] value(); 
}
