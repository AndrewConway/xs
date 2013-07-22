/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.api.dependency;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Error check functions and others may have arguments. These will be applied via dependency injection
 * from some elements set for the class.
 * 
 * You can add some more possible objects to inject by marking a function with this annotation. 
 * The output of this function (or outputs if the result is a collection) will then be available
 * for dependency injection for this object and any children.
 * 
 * Ordering is done intelligently so that if m1 and m2 are member functions in a class, and m2 produces something needed by m1, then m1 will go first.
 * As the type of the result is not necessarily known at the start, the following algorithm is used
 *   (1) Find some function that can be done. Do it. Add the results to the dependency injection.
 *   (2) Go back to step 1.
 *   
 * Note that if the results are a collection or array or option, then the subelements will be added (not recursively)
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface DependencyProvider {
	
}
