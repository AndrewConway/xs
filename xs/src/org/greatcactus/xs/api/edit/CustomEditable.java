/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Tell XS that there should be a custom gui component for this object.
 * 
 * This is normally applied to a method in the object. That method may be a dummy one, just
 * something to hang the annotation on (this us up to the component). The name of
 * the function will be used in the normal way for labels, visibility, etc., like
 * @ExtraDisplayField.
 * 
 * You will need to register a custom drawer (type CustomComponent)
 * for the object with the appropriate
 * client driver (e.g. if you are using the normal HTML5 client, then with
 * HTML5DetailsPane.addCustom() )
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD,ElementType.TYPE})
public @interface CustomEditable {
	/** The name of the custom component */
	String value();
}
