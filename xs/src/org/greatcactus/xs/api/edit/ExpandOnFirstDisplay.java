/**
 * Copyright Andrew Conway 2008-2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 *For a field marked IndividuallyEditable, make that when this should be first displayed, all children should
 *also be displayed. See also {@link PropagateAddToExisting} which is often used in association with this annotation.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface ExpandOnFirstDisplay {
}
