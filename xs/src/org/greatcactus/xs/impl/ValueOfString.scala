/**
 * Copyright Andrew Conway 2012. All rights reserved.
 */
package org.greatcactus.xs.impl

import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer

/**
 * A class that turns a string into an object. This should do the opposite of the toString function.
 *
 */

abstract class ValueOfString { def apply(s:String) : AnyRef }

object ValueOfString {
/*
  def deserializer[T](c:java.lang.Class[T]) : ValueOfString[T] = {
    c.getName match {
      case "byte"       => ValueOfStringByte.asInstanceOf[ValueOfString[T]] 
      case "short"      => ValueOfStringShort.asInstanceOf[ValueOfString[T]] 
      case "char"       => ValueOfStringChar.asInstanceOf[ValueOfString[T]] 
      case "int"        => ValueOfStringInt.asInstanceOf[ValueOfString[T]] 
      case "long"       => ValueOfStringLong.asInstanceOf[ValueOfString[T]] 
      case "float"      => ValueOfStringFloat.asInstanceOf[ValueOfString[T]] 
      case "double"     => ValueOfStringDouble.asInstanceOf[ValueOfString[T]] 
      case "boolean"    => ValueOfStringBoolean.asInstanceOf[ValueOfString[T]]  
      case "scala.Byte"       => ValueOfStringByte.asInstanceOf[ValueOfString[T]] 
      case "scala.Short"      => ValueOfStringShort.asInstanceOf[ValueOfString[T]] 
      case "scala.Char"       => ValueOfStringChar.asInstanceOf[ValueOfString[T]] 
      case "scala.Int"        => ValueOfStringInt.asInstanceOf[ValueOfString[T]] 
      case "scala.Long"       => ValueOfStringLong.asInstanceOf[ValueOfString[T]] 
      case "scala.Float"      => ValueOfStringFloat.asInstanceOf[ValueOfString[T]] 
      case "scala.Double"     => ValueOfStringDouble.asInstanceOf[ValueOfString[T]] 
      case "scala.Boolean"    => ValueOfStringBoolean.asInstanceOf[ValueOfString[T]]  
      case "java.lang.String" => ValueOfStringString.asInstanceOf[ValueOfString[T]]  
      case _ =>
        throw new IllegalArgumentException
    }
  }
 */
  

  /**
   * Get an appropriate deserializer for the given class, or produce an IllegalArgumentException.
   */
  def deserializer(c:java.lang.Class[_]) : ValueOfString = cache.getOrElseUpdate(c,deserializerUncached(c))

  private def cache = new scala.collection.mutable.HashMap[java.lang.Class[_],ValueOfString]
    
  private def deserializerUncached(c:java.lang.Class[_]) : ValueOfString = {
    c.getName match {
      case "byte"       => ValueOfStringByte 
      case "short"      => ValueOfStringShort
      case "char"       => ValueOfStringChar 
      case "int"        => ValueOfStringInt
      case "long"       => ValueOfStringLong
      case "float"      => ValueOfStringFloat
      case "double"     => ValueOfStringDouble 
      case "boolean"    => ValueOfStringBoolean  
      case "scala.Byte"       => ValueOfStringByte 
      case "scala.Short"      => ValueOfStringShort 
      case "scala.Char"       => ValueOfStringChar 
      case "scala.Int"        => ValueOfStringInt 
      case "scala.Long"       => ValueOfStringLong 
      case "scala.Float"      => ValueOfStringFloat
      case "scala.Double"     => ValueOfStringDouble 
      case "scala.Boolean"    => ValueOfStringBoolean  
      case "java.lang.String" => ValueOfStringString  
      case _ =>
        // see if there is a Scala style companion object with an apply(String) function 
        searchForCompanionObjectMethod(c,"apply",List("java.lang.String")) match {
          case Some(m) => return new ValueOfStringCompanionApply(m)
          case None => 
        }
        // see if there is a Java style static valueOf(String) function
        try {
          val javaStaticValueOfMethod = c.getMethod("valueOf",classOf[java.lang.String])
          if (javaStaticValueOfMethod!=null && java.lang.reflect.Modifier.isStatic(javaStaticValueOfMethod.getModifiers)) return new ValueOfStringJavaStaticValueOf(javaStaticValueOfMethod)      
        } catch { case _:java.lang.NoSuchMethodException => }
        // see if there is a constructor taking a string.
        try {
          val simpleConstructor = c.getConstructor(classOf[java.lang.String]).asInstanceOf[java.lang.reflect.Constructor[_ <: AnyRef]]
          if (simpleConstructor!=null) return new ValueOfStringStringConstructor(simpleConstructor)
        } catch { case _:java.lang.NoSuchMethodException => }
        
        throw new IllegalArgumentException
    }
  }
  
  import scala.reflect.runtime._

  /**
   * Search the companion object for class c for a method called desiredName with argument list length equal to desiredArgs.length with the full name of the type of each arg given by desiredArgs.
   */
  def searchForCompanionObjectMethod(c:java.lang.Class[_],desiredName:String,desiredParams:List[String]) : Option[universe.MethodMirror] = {
          // see if there is a Scala style companion object with an apply(String) function 
        val rootMirror = universe.runtimeMirror(c.getClassLoader)
        val classSymbol = rootMirror.classSymbol(c)
        val classMirror = rootMirror.reflectClass(classSymbol)
        try {
          val moduleSymbol = rootMirror.moduleSymbol(c)
          //println("module symbol="+moduleSymbol)
          val moduleMirror = rootMirror.reflectModule(moduleSymbol)
         // println("module mirror="+moduleMirror)
          val symbol = moduleMirror.symbol // is this the same as moduleSymbol???
          if (symbol.moduleClass.isClass) {
            //println("Found companion "+symbol)
            val applyMethods = symbol.moduleClass.asClass.toType.member(universe.newTermName(desiredName))
            //println("Found apply method(s) "+applyMethods)
            if (applyMethods!=null && applyMethods.isTerm) for (applyMethod<-applyMethods.asTerm.alternatives;params <- applyMethod.asMethod.paramss.headOption.orElse(Some(List()))) {
              // check to see if takes a string
              val actualParams : List[String] = params.map{_.typeSignature.typeSymbol.fullName}.toList
              if (actualParams==desiredParams) {
                val instanceMirror = rootMirror.reflect(moduleMirror.instance)
                val methodMirror = instanceMirror.reflectMethod(applyMethod.asMethod)
                return Some(methodMirror)
              }
            }
          }
        } catch { case e : Exception => /* e.printStackTrace */ }
        None
  }
  
  private object ValueOfStringByte extends ValueOfString { def apply(s:String) = java.lang.Byte.valueOf(s) }
  private object ValueOfStringShort extends ValueOfString { def apply(s:String) = java.lang.Short.valueOf(s) }
  private object ValueOfStringChar extends ValueOfString { def apply(s:String) = java.lang.Character.valueOf(s.charAt(0)) }
  private object ValueOfStringInt extends ValueOfString { def apply(s:String) = java.lang.Integer.valueOf(s) }
  private object ValueOfStringLong extends ValueOfString { def apply(s:String) = java.lang.Long.valueOf(s) }
  private object ValueOfStringFloat extends ValueOfString { def apply(s:String) = java.lang.Float.valueOf(s) }
  private object ValueOfStringDouble extends ValueOfString { def apply(s:String) = java.lang.Double.valueOf(s) }
  private object ValueOfStringBoolean extends ValueOfString { def apply(s:String) = java.lang.Boolean.valueOf(s) }
  private object ValueOfStringString extends ValueOfString { def apply(s:String) = s }

  private class ValueOfStringJavaStaticValueOf(javaStaticValueOfMethod:java.lang.reflect.Method) extends ValueOfString { def apply(s:String) = javaStaticValueOfMethod.invoke(null,s) }
  private class ValueOfStringStringConstructor(simpleConstructor:java.lang.reflect.Constructor[_ <: AnyRef]) extends ValueOfString { def apply(s:String) = simpleConstructor.newInstance(s) }
  private class ValueOfStringCompanionApply(method:scala.reflect.runtime.universe.MethodMirror) extends ValueOfString { def apply(s:String) = method.apply(s).asInstanceOf[AnyRef] }

      /** Given a string representation of the object, get the object. This also works for semicolon separated lists */
  /*
  def getValuesFromSemicolonSeparatedString(stringrep:String,decoder:ValueOfString) : ArrayBuffer[AnyRef] = {
    val textreps : Array[String] =CollectionStringUtil.separateSemicolonListEscaped(stringrep) 
    val buffer = new ArrayBuffer[AnyRef](textreps.length)
    for (s<-textreps) buffer+=decoder(s)
    buffer
  }
*/
  
  
}