/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.display;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * You can make "synthetic", read only fields from methods by annotating them with this annotation.
 * These fields cannot be referenced from elsewhere (eg errorchecking). The name for the localization
 * resources is the name of the method this annotates.
 * 
 * The method produced by this field can return the same things as an @LabelProvider.
 * 
 * Other relevant annotations are @Multiline, @XSIcon, @WidthHing, @OrderingPriority, @OnlyReleventIf, @EditSection, @HideName
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface ExtraDisplayField {
}
