/**
 * Copyright Andrew Conway 2014. All rights reserved.
 */
package org.greatcactus.xs.api.serialization;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Normally in JSON serialization, empty collections will have the hole field left out.
 * This annotation says leave them in. May be on a field or a type (in which case it applies to all fields).
 * 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD,ElementType.TYPE})
public @interface IncludeEmptyCollections {
}
