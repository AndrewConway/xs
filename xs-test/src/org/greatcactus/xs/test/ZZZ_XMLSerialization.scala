/**
 * Copyright Andrew Conway 2012-2013. All rights reserved.
 */
package org.greatcactus.xs.test

import org.greatcactus.xs.api.serialization._
import org.greatcactus.xs.api._
import org.junit.Assert._
import org.junit.Test
import scala.annotation.ClassfileAnnotation
import scala.reflect.ClassTag
import org.greatcactus.xs.impl.SerializableTypeInfo
import org.greatcactus.xs.api.XS
import org.greatcactus.xs.api.XSSubclasses
import org.greatcactus.xs.impl.NotSerializableException
import org.greatcactus.xs.impl.XSSpecificationError



 /**
 * Test XML serialization and deserialization.
 */
class ZZZ_Serialization {

  @Test
  def test1 {
    val t = new Test1("Flintstone",List("Rubble"))
    //println("Ser=["+new String(XMLSerialize.serializeToString(t))+"]")
    assertEquals("""<Test1 fred="Flintstone" barney="Rubble"/>""",new String(XMLSerialize.serializeToByteArray(t)))
    val m = XMLDeserialize.deserialize[Test1]("""<Test1 fred="Flintstone" barney="Rubble"></Test1>""".getBytes)
    assertEquals("Flintstone",m.fred)
    assertEquals(Seq("Rubble"),m.barney)
    assertEquals("""<Test1 fred="Flintstone" barney="Rubble"/>""",new String(XMLSerialize.serializeToByteArray(m)))
  }

  @Test
  def test2 {
    val t = new Test2(4,3.6,Array(5,4,3))
    assertEquals("""<Test2 f1="4" f2="3.6" f3="5;4;3"/>""",new String(XMLSerialize.serializeToByteArray(t)))
    val m = XMLDeserialize.deserialize[Test2]("""<Test2 f1="4" f2="3.6" f34="5;4;3"/>""".getBytes)
    assertEquals(4,m.f1)
    assertEquals(3.6,m.f2,1e-10)
    assertEquals(3,m.f3.length)
    assertEquals(5,m.f3(0))
    assertEquals(4,m.f3(1))
    assertEquals(3,m.f3(2))
    assertEquals("""<Test2 f1="4" f2="3.6" f3="5;4;3"/>""",new String(XMLSerialize.serializeToByteArray(m)))
  }
  
  @Test
  def test3 {
    val t = new Test3(List(5,4,3))
    assertEquals("""<Test3 f1="5;4;3"/>""",new String(XMLSerialize.serializeToByteArray(t)))
    val m = XMLDeserialize.deserialize[Test3]("""<Test3 f1="5;4;3"></Test3>""".getBytes)
    assertEquals(List(5,4,3),m.f1)
    assertEquals("""<Test3 f1="5;4;3"/>""",new String(XMLSerialize.serializeToByteArray(m)))
    
  }

  @Test
  def test4 {
    val t = new Test4("Flintstone",List("Rub>ble"))
    //println("Ser=["+new String(XMLSerialize.serializeToString(t))+"]")
    assertEquals("""<Test4><hero>Flintstone</hero><barney>Rub&gt;ble</barney></Test4>""",new String(XMLSerialize.serializeToByteArray(t)))
    val m = XMLDeserialize.deserialize[Test4]("""<Test4><hero>Flintstone</hero><barney>Rub&gt;ble</barney></Test4>""".getBytes)
    assertEquals("Flintstone",m.fred)
    assertEquals(List("Rub>ble"),m.barney)
    assertEquals("""<Test4><hero>Flintstone</hero><barney>Rub&gt;ble</barney></Test4>""",new String(XMLSerialize.serializeToByteArray(m)))
    test("<Test4><hero>Two\nLines</hero></Test4>",new Test4("Two\nLines",Nil)) 
  }

  def test[T <: AnyRef :ClassTag](expectedXML:String,obj:T) {
    assertEquals(expectedXML,new String(XMLSerialize.serializeToByteArray(obj)))
    val m = XMLDeserialize.deserialize[T](expectedXML.getBytes)
    assertEquals(expectedXML,new String(XMLSerialize.serializeToByteArray(m)))
  }
  
  @Test
  def test5 {
    val t1 = new Test1("Flintstone",List("Rubble"))
    val t2a = new Test2(4,3.6,Array(5,4,3))
    val t2b = new Test2(0,-1.6,null)
    test("""<T5><Test1 fred="Flintstone" barney="Rubble"/></T5>""",new Test5(t1,null))
    test("""<T5><Test1 fred="Flintstone" barney="Rubble"/><Test2 f1="4" f2="3.6" f3="5;4;3"/><Test2 f1="0" f2="-1.6"/></T5>""",new Test5(t1,Array(t2a,t2b)))
    test("""<T5><Test1 fred="Flintstone" barney="Rubble"/><Test2 f1="4" f2="3.6" f3="5;4;3"/><null-f2/><Test2 f1="0" f2="-1.6"/></T5>""",new Test5(t1,Array(t2a,null,t2b)))
  }
  
  @Test
  def test6 { // test string deserialization using static java valueOf
    test("""<Test6 i="3"/>""",new Test6(new java.lang.Integer(3)))
  }

  @Test
  def test7 { // test string deserialization using scala companion object
    test("""<Test7 f="3"/>""",new Test7(new Test7a(3)))
  }

  @Test
  def test8 { // test string deserialization using constructor
    test("""<Test8 f="3"/>""",new Test8(new Test8a("3")))
  }
  
  @Test
  def test9 {
     test("""<Test9><Poly1/></Test9>""",new Test9(new Poly1))
     test("""<Test9><P2/></Test9>""",new Test9(new Poly2))
     test("""<Test9><Poly4/></Test9>""",new Test9(new Poly4))
  }

  @Test
  def test10 {
    shouldFail[Test10Fail]
    test("""<Test10Pass><p1><Poly1/></p1><p2><P2/></p2></Test10Pass>""",new Test10Pass(new Poly1,new Poly2))
  }
   
  @Test
  def test11 { // test cyclic structures
     test("""<Test11><Test11/></Test11>""",new Test11(List(new Test11(Nil))))
     test("""<Test11><Test11/><Test11><Test11/></Test11></Test11>""",new Test11(List(new Test11(Nil),new Test11(List(new Test11(Nil))))))
  }

  @Test
  def test12 { // test optional structures
     test("""<Test12><Test12/></Test12>""",new Test12(Some(new Test12(None))))
  }
  
  @Test
  def testA1 { // test arrays of null as an attribute
    test("""<TestA1 a="5;;4"/>""",new TestA1(Array(5,null,4)))
  }

  def shouldFail[T:ClassTag] {
    try {
      val res = SerializableTypeInfo.get(scala.reflect.classTag[T].runtimeClass)
      fail("Trying to get deserialization type information for "+scala.reflect.classTag[T].runtimeClass+" should have thrown an XSSpecificationError but instead returned "+res)
    } catch { case _: XSSpecificationError => }
  }
  
  @Test
  def testFail {
    assertEquals(None,SerializableTypeInfo.get(classOf[TestF1]))
    shouldFail[TestF2]
    shouldFail[TestF3]
  }
  
  @Test
  def test13 { // test optional strings
     test("""<Test13 t1="fr;ed"><t2>bar;ney</t2></Test13>""",new Test13(Some("fr;ed"),Some("bar;ney")))
     test("""<Test13/>""",new Test13(None,None))
  }
}


@XS class Test1 (@XSSerializeAsAttribute val fred:String,@XSSerializeAsAttribute val barney:Seq[String])


@XS
class Test2 (
    val f1:Int,
    val f2:Double,
    @XSObsoleteName(Array("f33","f34")) val f3:Array[Int]
)

@XS
class Test3 (
    val f1:List[Int]
)

@XS
class Test4 @XSConstructor() (
    @XSName("hero") val fred:String,
    val barney:List[String]
) {
  def this(s:String) = this(s,Nil)
  def distration = barney
}

@XS
@XSName("T5")
class Test5 (
    val f1 : Test1,
    val f2 : Array[Test2]
  )
  
@XS
class Test6(val i:java.lang.Integer)

@XS
class Test7(val f:Test7a)

class Test7a(i:Int) {
  override def toString = i.toString
}
object Test7a {
  def apply(s:String) = new Test7a(s.toInt)
}

@XS
class Test8(val f:Test8a)

class Test8a(s:String) {
  override def toString = s
}

@XS
@XSSubclasses(Array(classOf[Poly1],classOf[Poly2],classOf[Poly3]))
trait Poly

@XS
class Poly1 extends Poly
@XS @XSName("P2")
class Poly2 extends Poly
@XS
@XSSubclasses(Array(classOf[Poly4],classOf[Poly5]))
class Poly3 extends Poly
@XS
class Poly4 extends Poly3
@XS
class Poly5 extends Poly3

@XS
class Test9(val p:Poly)

@XS
class Test10Fail(val p1:Poly,val p2:Poly)

@XS
class Test10Pass(@XSWrapper val p1:Poly,@XSWrapper val p2:Poly)

@XS
class Test11(val t:List[Test11])

@XS
class Test12(val t:Option[Test12])

@XS
class Test13(@XSSerializeAsAttribute val t1:Option[String],@XSSerializeAsBlock val t2:Option[String])

@XS
class TestA1(val a:Array[java.lang.Integer])

// Things that should fail.

class TestF1(val x:Int)

@XS
class TestF2(val f:TestF1)

@XS
class TestF3(val f1:Int,@XSName("f1") val f2:Int)


