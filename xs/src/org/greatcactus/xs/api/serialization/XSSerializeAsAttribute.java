/**
 * Copyright Andrew Conway 2006-2013. All rights reserved.
 */
package org.greatcactus.xs.api.serialization;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * <p>This tag should be added to a field that would normally be serialized as a block to make
 * it instead be serialized in an XML attribute. It is the converse of @XSSerializeAsBlock.
 * Note that the only type that this actually makes sense for is a String, as that is the only
 * type serializable as an attribute that is not by default serialized as an attribute.</p> 
 *
 * Note that attributes in XML are not whitespace friendly - blocks of whitespace will be turned into a single
 * space. This is the reason why strings are not by default stored as attributes.
 * 
 * <p>Note that if it is a collection, then each element of the collection will be serialized inside a separate block. </p>
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface XSSerializeAsAttribute {
}