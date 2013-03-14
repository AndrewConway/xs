/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.dependency;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Improve efficiency of dependency injection by reducing recomputation. If a class C contains fields fx, fy and fz, then a
 * change in any of those fields will produce a new object of type C, and will require recomputation of all the dependencies.
 * However, if a dependency only depends on field fx, then you can add the annotation @OnlyAffectedByFields({"fx"}) and
 * then the result is not recomputed if only fy or fz change. Of course, if the function has arguments itself, and they change,
 * then it will still be recomputed. If the list of affected fields is blank, then the function is assumed to be independent
 * of the object.
 * 
 * <p>
 * But what if you have a field fx which itself is a class with subfields, and it only depends upon one of those subfields sf2,
 * and you really want to be efficient? You can handle this situation with a two step process: First make a @DependencyProvider
 * function in C that makes a wrapper around C.fx.sf2. Make sure that this wrapper has appropriate equals and hashCode methods.
 * Then make a second @DependencyProvider function in C that takes just that wrapper and performs the expensive computation
 * you are trying to minimize. It should have the @OnlyAffectedByFields annotation with no arguments, which means it only
 * will be called when the wrapper changes. 
 * </p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface OnlyAffectedByFields {
	String[] value();
}
