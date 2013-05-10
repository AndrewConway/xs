/**
 * Copyright Andrew Conway 2013. All rights reserved.
 */
package org.greatcactus.xs.api.command;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Extra annotation to put on a method that you want to be able to run as a command. Method may take a ProgressMonitor,
 * and any available dependency injection information. 
 * 
 * It may produce a CommandResult which will indicate whether it worked. Otherwise it is assumed to have worked.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface XSCommand {
	
}
