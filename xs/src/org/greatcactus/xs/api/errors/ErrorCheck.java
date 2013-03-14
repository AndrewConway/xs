/**
 * Copyright Andrew Conway 2008-2013. All rights reserved.
 */
package org.greatcactus.xs.api.errors;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Define a function that is will check for errors. The function should return
 * either null, a ErrorDetails object, an array of ErrorDetails objects, or a collection
 * of ErrorDetails objects.
 * 
 * <p>The method may optionally have arguments. In this case, the values are taken
 * from the dependency injection system</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface ErrorCheck {
	/** The field associated with this error, if any. The field should be named with its title. Sections can also be used - if so, they are called "Section.name" where name is replaced by the name of the section. */
	String value() default "";
}
