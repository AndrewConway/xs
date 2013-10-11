/**
 * Copyright Andrew Conway 2011-2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * <p>Represent a tag that should be displayed when editing, but not allowed to be edited. </p>
 * 
 * You would use some other (additional) display annotation, such as @StringEditable.
 * 
 * Note that this is poorly implemented at the moment; it only works with @StringEditable text (not selections).
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface DisplayOnly {
}
