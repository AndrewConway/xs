/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.edit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Represent a tag that should be edited as an image.
 * 
 * The result is edited as a string, and encodes the image as a URL. This may be a data URL or a network URL. If you always want to canonicalize to a data URL, see {@link ResolveNetworkReferences}.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface ImageEditable {
}
