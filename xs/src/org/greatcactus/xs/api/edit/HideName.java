/**
 * Copyright Andrew Conway 2011-2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * <p>Represent a tag that should be displayed without a name. </p>
 * 
 * You would use some other (additional) display annotation, such as @StringEditable.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface HideName {
}
