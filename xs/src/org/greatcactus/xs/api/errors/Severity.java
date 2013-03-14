/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.api.errors;


/**
 * How severe an error is
 */
public enum Severity {
	ERROR,WARNING,INFO;

	public int level() { 
	  switch (this) {
		case ERROR : return 0;
		case WARNING : return 1;
		case INFO: return 2;
	  }
	  throw new IllegalArgumentException(this.toString());
	}
	
	public static Severity max(Severity a, Severity b) {
		if (a==null) return b;
		if (b==null) return a;
		if (a==INFO) return b;
		if (b==INFO) return a;
		if (a==WARNING) return b;
		if (b==WARNING) return a;
		assert a==ERROR;
		assert b==ERROR;
		return ERROR;
	}
}
