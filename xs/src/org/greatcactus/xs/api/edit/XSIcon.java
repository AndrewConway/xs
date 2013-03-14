/**
 * Copyright Andrew Conway 2008-2012. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * An icon associated with this class/field. The string is a reference to a name that is a section in an xsicons.manifest file. 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD,ElementType.TYPE})
public @interface XSIcon {
	/** the name of the image */
	String value() ;
}
