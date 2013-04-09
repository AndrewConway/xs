/**
 * Copyright Andrew Conway 2006-2013. All rights reserved.
 */
package org.greatcactus.xs.api;

import java.lang.annotation.*;

/**
 * <p>This tag should be put on any class that is desired to be
 * serialized using XS.</p>
 * 
 * <p>The class should have a public constructor taking some number of fields.
 * Each of those fields should have an accessor function with the same
 * name. The class will be serialized by writing out those fields, and
 * deserialized by reading them in and then calling the public constructor. </p>
 * 
 * <p>A field may be either serialized as a string, or a class itself using
 * the above constructor. It is done as a class iff that class has an 
 * XSSerializable tag. It may furthermore be implemented as an XML
 * tag or as an attribute. If it is serialized as a string (other than, ironically, 
 * fields of class java.lang.String), AND there is
 * no {@link XSSerializeAsBlock} it will be serialized as an attribute;
 * otherwise it will be serialized as a tag. (java.lang.String is not by
 * default serialized as an attribute to preserve whitespace. If you want
 * to serialize a string as an attribute see {@link @XSSerializeAsAttribute).</p>
 * 
 * <p>If a field is serialized as an attribute, the attribute's name 
 * will be the name of the field, unless the {@link XSName} annotation
 * is on it, in which case it will use the name there given.</p>
 * 
 * <p>If a field is serialized as a tab, then the situation is more
 * complex. One solution is to use the name of the class being serialized.
 * This does not work if there are two fields with the same type. Another
 * solution is to use the name of the field. This does not work if the
 * type being serialized has subtypes, in which case it will be difficult
 * to work out the type of the subtypes.</p>
 * 
 * <p>The actually used algorithm is somewhat of a hack, checking the following things in order
 * <ul>
 *   <li>If the field has {@link XSHolder} annotation, then the field name 
 *       (possibly overridden with {@link XSName}) is used as a
 *       wrapper around the actual serialization, which then uses the class name</li>
 *   <li>If the field has {@link XSName} annotation, then that is used.</li>
 *   <li>Otherwise the class name is used.</li>
 * </ul></p>
 * 
 * <p>If an object is null, then the corresponding name or tag is not included.</p>
 * 
 * <p>If a collection is being serialized then there are multiple tags 
 * (possibly all inside some other tag if {@link XSHolder} is used).
 * If an element of a collection is null, then a null tag of null-fieldname is used.
 * If it is a collection of a collection, the inner elements are put inside
 * a tag of list-fieldname. Note that this means that for field of type Option[X]
 * where X is a simple class, then it will be serialized exactly the same way as
 * if the field were X itself.</p>
 * 
 * <p>Note that this means that zero length collections and null pointers are
 * serialized to the same thing; during deserialization it is assumed to be a 
 * zero length collection.</p>
 * 
 * <p>A collection of items that is serialized as an attribute is semicolon separated.
 * Semicolons thus need to be escaped; they are escaped by the back quote ` as
 * are back quotes themselved.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface XS {
	
}
