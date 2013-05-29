/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Tell XS that there should be a semi-custom gui component for this object.
 * 
 * This is typically added to a StringEditable field, to indicate that there should
 * be some sort of popup associated with this field that opend up a custom editor.
 * You could do the same thing with more work using the fully CustomEditable infrastructure.
 *  
 * You will need to register a custom drawer (type CustomComponent)
 * for the object with the appropriate
 * client driver (e.g. if you are using the normal HTML5 client, then with
 * HTML5DetailsPane.addCustom() )
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD,ElementType.TYPE})
public @interface CustomPopup {
	/** The name of the custom popup component */
	String value();
}
