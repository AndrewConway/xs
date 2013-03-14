/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 */
package org.greatcactus.xs.impl

import org.greatcactus.xs.frontend.XSTreeNode

/**
 * Code to check for the uniqueness of XS field values (to implement the ErrorIfNotUnique annotation)
 */
class UniquenessCheckInfo(val key:UniquenessClass,val severity:Option[String])

/** Uniqueness is generally across instances of the same XSField, but can also be across a string. This merges these two possible keys. */
class UniquenessClass(val key:AnyRef) extends AnyVal {
  override def toString() = key.toString
}

class FieldsToCheckForUniquessForGivenKey(val key:UniquenessClass,val fields:List[XSFieldInfo])

object FieldsToCheckForUniqueness {
  val empty = new FieldsToCheckForUniqueness(Nil)
}
class FieldsToCheckForUniqueness(val classes:List[FieldsToCheckForUniquessForGivenKey]) {
  def get(obj:AnyRef) : AllElementsByClass = {
    val map = Map.empty++(
      for (c<-classes) yield c.key->{
        var res : AllElements[AnyRef] = AllElements.empty
        for (field<-c.fields;elem<-field.getAllFieldElements(obj)) if (elem!=null) res=res.plus(elem.asInstanceOf[AnyRef])
        res
      }
    )
    new AllElementsByClass(map)
  }
  def isEmpty = classes.isEmpty
}

object UniquenessCheckResolution {
  val getGlobalBadness : XSTreeNode=>NonuniqueElements = node => node.root.uniquenessCheckResolution.getBadGlobal
  val getLocalBadness : XSTreeNode=>NonuniqueElements = node => node.uniquenessCheckResolution.getBadLocal
  val getParentBadness : XSTreeNode=>NonuniqueElements = node => if (node.parent==null) NonuniqueElements.empty else node.parent.uniquenessCheckResolution.getBadParent
}
class UniquenessCheckResolution(val node:XSTreeNode,val local:FieldsToCheckForUniqueness,val parent:FieldsToCheckForUniqueness,val global:FieldsToCheckForUniqueness) {
  private var dirty = true
  
  private var localNonUnique = NonuniqueElements.empty
  private var parentNonUnique = NonuniqueElements.empty
  private var dataForParent = AllElementsByClass.empty
  private var dataForGlobal = AllElementsByClass.empty
  private var globalNonUnique = NonuniqueElements.empty // only meaningful if this is the root node.
   
  def getDataForParent : AllElementsByClass = synchronized { clean(); dataForParent }
  def getDataForGlobal : AllElementsByClass = synchronized { clean(); dataForGlobal }
  
  private def getBadGlobal : NonuniqueElements = synchronized { clean(); globalNonUnique }
  private def getBadParent : NonuniqueElements = synchronized { clean(); parentNonUnique }
  private def getBadLocal : NonuniqueElements = synchronized { clean(); localNonUnique }
  
  def invalidate() {
   // println("UniquenessCheckResolution.invalidate("+node+")")
    synchronized { dirty=true; }
  }
  
  def clean() {
    //println("UniquenessCheckResolution.clean("+node+")")
    synchronized {
      if (dirty) {
        dirty=false;
        localNonUnique=local.get(node.getObject).justNonUnique
        dataForParent=parent.get(node.getObject)
        val oldParentData = parentNonUnique
        var parentCumSum = AllElementsByClass.empty
        var globalCumSum = global.get(node.getObject)
        for (c<-node.allChildren) {
           parentCumSum+=c.uniquenessCheckResolution.getDataForParent
           globalCumSum+=c.uniquenessCheckResolution.getDataForGlobal
        } 
        parentNonUnique=parentCumSum.justNonUnique
        //println("Node "+node.toString)
        //println("Parent Cum Sum"+parentCumSum)
        //println("Global Cum Sum"+globalCumSum)
        dataForGlobal=globalCumSum
        if (parentNonUnique!=oldParentData) { // send out recomputation
          for (c<-node.allChildren) c.parentUniquenessErrorsChanged()
        }
        if (node.isRoot) {
          val oldGlobalNonUnique = globalNonUnique
          globalNonUnique = dataForGlobal.justNonUnique
          if (oldGlobalNonUnique!=globalNonUnique) { // send out recomputation messages
            node.globalUniquenessErrorsChanged()
          }
        }
      }
    }
  }
}

object NonuniqueElements {
  val empty = new NonuniqueElements(Map.empty)
}
/** The set of non-unique elements, divided up by class. */
class NonuniqueElements(val map:Map[UniquenessClass,Set[AnyRef]]) extends AnyVal {
  //def this(extendedMap:Map[UniquenessClass,AllElements[AnyRef]]) = this(extendedMap.map{case (key,el) => (key,el.moreThanOnce)})
  //override def equals(other:Any) = other map {
  //  case o:NonuniqueElements => 
  //}
}

object AllElementsByClass {
  val empty = new AllElementsByClass(Map.empty)
}
class AllElementsByClass(val map:Map[UniquenessClass,AllElements[AnyRef]]) extends AnyVal {
  def justNonUnique = new NonuniqueElements(map.map{case (key,el) => (key,el.moreThanOnce)})
  def + (other:AllElementsByClass) : AllElementsByClass = {
    if (map.isEmpty) other
    else if (other.map.isEmpty) this
    else {
       var res = map
       for ((key,value)<-other.map) res+=key->(res.get(key) match { case None => value; case Some(already) => already.union(value)})
       new AllElementsByClass(res)
    }
  }
  override def toString() = (for ((key,elems)<-map) yield key.toString+"->"+elems.toString).mkString(";")
}
/*
class ElementsThatShouldBeUnique(compute : => Map[UniquenessClass,AllElements[AnyRef]],onChange:Option[()=>Unit]=None) {
  private[this] var map : Option[Map[UniquenessClass,AllElements[AnyRef]]] = None
  def get : Map[UniquenessClass,AllElements[AnyRef]] = synchronized {
    if (!map.isDefined) map=Some(compute)
    map.get
  } 
  def invalidate() { synchronized { map=None }}
}
*/
object AllElements {
  val empty = new AllElements[AnyRef](Set.empty,Set.empty)
}

/** Like a set, except keep track of elements that are in it once, or more than once. */
class AllElements[T](val atLeastOnce:Set[T],val moreThanOnce:Set[T]) {
  def union(other:AllElements[T]) = new AllElements(atLeastOnce.union(other.atLeastOnce),moreThanOnce.union(other.moreThanOnce).union(atLeastOnce.intersect(other.atLeastOnce)))
  def plus(e:T) = if (atLeastOnce.contains(e)) new AllElements(atLeastOnce,moreThanOnce+e) else new AllElements(atLeastOnce+e,moreThanOnce)
  override def toString() = "Once:"+(atLeastOnce.mkString(","))+" Twice:"+(moreThanOnce.mkString(","))
}

