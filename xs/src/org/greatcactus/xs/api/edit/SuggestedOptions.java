/**
 * Copyright Andrew Conway 2008-2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * State that a field has a finite number of suggested options that are listed here. This is generally poorly supported by clients.
 * 
 * This can be on a field, in which case it applies to that particular field.
 * It can be on a class, in which case it applies to all options of that class.
 * 
 * For internationalization,
 *   if on a field, then if the field is F and an options is O, then the localization tag should be F.enum.O
 *   if on a class, then the localization tag should be enum.O 
 * 
 * @see {@link SuggestedOptions}
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface SuggestedOptions {
	/** If there are standard options, list them. This will be obtained implicitly for enumerations; you don't need to set it. */
	String[] value() default {};
}
