/**
 * Copyright Andrew Conway 2006-2013. All rights reserved.
 */
package org.greatcactus.xs.api.serialization;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * <p>This tag should be added to a field that would normally be serialized as a string
 * in an XML attribute. This makes it into a string in an XML block instead.</p>
 * 
 * <p>Note that if it is a collection, then each element of the collection will be serialized inside a separate block. </p>
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface XSSerializeAsBlock {
}