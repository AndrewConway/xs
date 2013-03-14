/**
 * Copyright Andrew Conway 2008-13. All rights reserved.
 */
package org.greatcactus.xs.api;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Sometimes you will have a polymorphic object to serialize. The type declaration
 * will be to some subtype, but actual objects will be of some other class. This
 * is problematic as that class does not know about all subclasses, so when a tag
 * is encountered, the field is not known. This annotation solves this problem
 * by explicitly listing the subclasses.
 * 
 * This annotation goes on the class that is subclassed, not on the field that 
 * contains an instance of that class.
 *
 * Note that if a subclass itself is subclassed, then the sub-subclasses may all
 * be entered on the ultimate parent, or they may be entered on intermediate classes
 * in their own tag with this name.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface XSSubclasses {
	Class<?>[] value();
}
