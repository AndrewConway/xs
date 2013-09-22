/**
 * Copyright (c) 2013 Silicon Econometrics Pty. Ltd. All rights reserved.
 */
package org.greatcactus.xs.api.edit

/**
 * If a @DependencyProvider function produces one of these, then it causes the editing object
 * to change, with the field "fieldname" updated to the new value.
 * 
 * Of course, you have to be very careful with these not to cause circular loops!
 * 
 * If fieldname==null then the whole object is replaced.
 */
class UpdateField(val fieldname:String,val newvalue:Any)