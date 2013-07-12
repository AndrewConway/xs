/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.util

import java.util.Calendar
import java.util.TimeZone

/**
 * Date (but not time). 1 Jan 2000 would be year=2000, month=1, day=1. 
 * Uses ISO 8601 - yyyy-mm-dd for serialization
 */
class SimpleDate(val year:Int,val month:Int,val day:Int) {

  override def toString = ""+year+"-"+month+"-"+day 
  
  /** Get the number of days between two dates */
  def - (other:SimpleDate) : Int = {
    val c1 = toGMTCal
    val c2 = other.toGMTCal
    var res = ((c1.getTimeInMillis-c2.getTimeInMillis)/(1000L*60*60*24)).toInt // may not be quite right due to leap milliseconds, but will be very close.
    c1.add(Calendar.DATE,res)
    // iterate up or down by a day. Mostly said code will not get called.
    while (c1.getTimeInMillis < c2.getTimeInMillis) { res+=1; c1.add(Calendar.DATE,1); }
    while (c1.getTimeInMillis > c2.getTimeInMillis) { res-=1; c1.add(Calendar.DATE,-1); }
    if (c1.getTimeInMillis!=c2.getTimeInMillis) throw new IllegalArgumentException("Internal error in date subtraction")
    res
  }
  
  /** Get a date with some number of days added. Argument can be negative. */
  def + (days:Int) : SimpleDate = {
    val cal = toGMTCal
    cal.add(Calendar.DATE,days)
    SimpleDate(cal)
  }
  
  
  def toGMTCal : Calendar = {
    val cal = Calendar.getInstance(SimpleDate.gmt)
    cal.set(Calendar.YEAR,year)
    cal.set(Calendar.MONTH,month-1)
    cal.set(Calendar.DAY_OF_MONTH,day)
    cal
  }
}

object SimpleDate {
  
  val gmt = TimeZone.getTimeZone("GMT")
  /** Convert a yyyy-mm-dd string to a date */
  def apply(s:String) : SimpleDate = {
    val split = s.split("-")
    if (split.length==3) new SimpleDate(split(0).toInt,split(1).toInt,split(2).toInt)
    else throw new IllegalArgumentException("Expecting date in yyyy-mm-dd format, got "+s)
  }
  
  /** Convert a java calendar to a simpledate */
  def apply(c:Calendar) : SimpleDate = new SimpleDate(c.get(Calendar.YEAR),c.get(Calendar.MONTH)+1,c.get(Calendar.DAY_OF_MONTH))
  
  def today : SimpleDate = {
    val cal = Calendar.getInstance() // local time zone
    cal.setTime(new java.util.Date) // now
    apply(cal)
  }
}