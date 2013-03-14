/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.dependency;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**

 * Extra annotation to put on a field already tagged with @DependencyProvider meaning that the result of it will be
 * carried forward to all children of the current class.
 * 
 * Actually it is legal to tag something with @PropagateToChildren but not @DependencyProvider. This means that the
 * result will be available to all children but not the current class. See @BlockDependencyInjection for an example
 * of when this would be useful. 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface PropagateToChildren {
	
}
