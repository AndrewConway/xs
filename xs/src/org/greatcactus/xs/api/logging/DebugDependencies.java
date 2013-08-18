/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.logging;

import java.lang.annotation.*;

/**
 * Tell an XS class to log dependency resolution status for instances of this class.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface DebugDependencies {
	
}
