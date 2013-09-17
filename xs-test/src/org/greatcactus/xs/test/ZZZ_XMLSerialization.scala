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
 * Test XML and JSON serialization and deserialization.
 */
class ZZZ_Serialization {

  @Test
  def test1xml {
    val t = new Test1("Flintstone",List("Rubble"))
    //println("Ser=["+new String(XMLSerialize.serializeToString(t))+"]")
    assertEquals("""<Test1 fred="Flintstone" barney="Rubble"/>""",new String(XMLSerialize.serializeToByteArray(t)))
    val m = XMLDeserialize.deserialize[Test1]("""<Test1 fred="Flintstone" barney="Rubble"></Test1>""".getBytes)
    assertEquals("Flintstone",m.fred)
    assertEquals(Seq("Rubble"),m.barney)
    assertEquals("""<Test1 fred="Flintstone" barney="Rubble"/>""",new String(XMLSerialize.serializeToByteArray(m)))
  }

  @Test
  def test1json {
    val t = new Test1("Flintstone",List("Rubble"))
    assertEquals("""{"xsType":"Test1","fred":"Flintstone","barney":["Rubble"]}""",new String(JSONSerialize.serializeToByteArray(t)))
    val m2 = JSONDeserialize.deserialize[Test1]("""{"xsType":"Test1","fred":"Flintstone","barney":["Rubble"]}""")
    assertEquals("Flintstone",m2.fred)
    assertEquals(Seq("Rubble"),m2.barney)
    assertEquals("""{"xsType":"Test1","fred":"Flintstone","barney":["Rubble"]}""",JSONSerialize.serializeToString(m2))
  }
  
  @Test
  def test1map {
    val t = new Test1("Flintstone",List("Rubble"))
    val expected = Map("type"->"Test1","fred"->"Flintstone","barney_len"->"1","barney_0"->"Rubble")
    assertEquals(expected,StringMapSerialize.serialize(t))
    val m2 = StringMapDeserialize.deserialize[Test1](expected)
    assertEquals("Flintstone",m2.fred)
    assertEquals(Seq("Rubble"),m2.barney)
    assertEquals(expected,StringMapSerialize.serialize(m2))
  }
  
  @Test
  def test1bson {
    val t = new Test1("Flintstone",List("Rubble"))
    val bson = MongoDBSerialize.serializeDBO(t)
    assertEquals("""{ "xsType" : "Test1" , "fred" : "Flintstone" , "barney" : [ "Rubble"]}""",bson.toString)
    val m2 = MongoDBDeserialize.deserialize[Test1](bson)
    assertEquals("Flintstone",m2.fred)
    assertEquals(Seq("Rubble"),m2.barney)
    assertEquals("""{"xsType":"Test1","fred":"Flintstone","barney":["Rubble"]}""",JSONSerialize.serializeToString(m2))
  }

  @Test
  def test2xml {
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
  def test2json {
    val t = new Test2(4,3.6,Array(5,4,3))
    assertEquals("""{"xsType":"Test2","f1":4,"f2":3.6,"f3":[5,4,3]}""",new String(JSONSerialize.serializeToByteArray(t)))
    val m2 = JSONDeserialize.deserialize[Test2]("""{"xsType":"Test2","f1":4,"f2":3.6,"f3":[5,4,3]}""")
    assertEquals(4,m2.f1)
    assertEquals(3.6,m2.f2,1e-10)
    assertEquals(3,m2.f3.length)
    assertEquals(5,m2.f3(0))
    assertEquals(4,m2.f3(1))
    assertEquals(3,m2.f3(2))
    assertEquals("""{"xsType":"Test2","f1":4,"f2":3.6,"f3":[5,4,3]}""",new String(JSONSerialize.serializeToByteArray(m2)))
  }
  @Test
  def test2map {
    val t = new Test2(4,3.6,Array(5,4,3))
    val expected = Map("type"->"Test2","f1"->"4","f2"->"3.6","f3_len"->"3","f3_0"->"5","f3_1"->"4","f3_2"->"3")
    assertEquals(expected,StringMapSerialize.serialize(t))
    val m2 = StringMapDeserialize.deserialize[Test2](expected)
    assertEquals(4,m2.f1)
    assertEquals(3.6,m2.f2,1e-10)
    assertEquals(3,m2.f3.length)
    assertEquals(5,m2.f3(0))
    assertEquals(4,m2.f3(1))
    assertEquals(3,m2.f3(2))
    assertEquals(expected,StringMapSerialize.serialize(m2))
  }
    
    
  @Test
  def test3xml {
    val t = new Test3(List(5,4,3))
    assertEquals("""<Test3 f1="5;4;3"/>""",new String(XMLSerialize.serializeToByteArray(t)))
    val m = XMLDeserialize.deserialize[Test3]("""<Test3 f1="5;4;3"></Test3>""".getBytes)
    assertEquals(List(5,4,3),m.f1)
    assertEquals("""<Test3 f1="5;4;3"/>""",new String(XMLSerialize.serializeToByteArray(m)))
  }

  @Test
  def test3json {
    val t = new Test3(List(5,4,3))
    assertEquals("""{"xsType":"Test3","f1":[5,4,3]}""",new String(JSONSerialize.serializeToByteArray(t)))
    val m = JSONDeserialize.deserialize[Test3]("""{"xsType":"Test3","f1":[5,4,3]}""".getBytes)
    assertEquals(List(5,4,3),m.f1)
    assertEquals("""{"xsType":"Test3","f1":[5,4,3]}""",new String(JSONSerialize.serializeToByteArray(m)))
  }

  @Test
  def test4xml {
    val t = new Test4("Flintstone",List("Rub>ble"))
    //println("Ser=["+new String(XMLSerialize.serializeToString(t))+"]")
    assertEquals("""<Test4><hero>Flintstone</hero><barney>Rub&gt;ble</barney></Test4>""",new String(XMLSerialize.serializeToByteArray(t)))
    val m = XMLDeserialize.deserialize[Test4]("""<Test4><hero>Flintstone</hero><barney>Rub&gt;ble</barney></Test4>""".getBytes)
    assertEquals("Flintstone",m.fred)
    assertEquals(List("Rub>ble"),m.barney)
    assertEquals("""<Test4><hero>Flintstone</hero><barney>Rub&gt;ble</barney></Test4>""",new String(XMLSerialize.serializeToByteArray(m)))
    test("<Test4><hero>Two\nLines</hero></Test4>","""{"xsType":"Test4","hero":"Two\nLines"}""",new Test4("Two\nLines",Nil)) 
  }

  @Test
  def test4json {
    val t = new Test4("Flintstone",List("Rub>ble"))
    //println("Ser=["+new String(XMLSerialize.serializeToString(t))+"]")
    assertEquals("""{"xsType":"Test4","hero":"Flintstone","barney":["Rub>ble"]}""",new String(JSONSerialize.serializeToByteArray(t)))
    val m = JSONDeserialize.deserialize[Test4]("""{"xsType":"Test4","hero":"Flintstone","barney":["Rub>ble"]}""".getBytes)
    assertEquals("Flintstone",m.fred)
    assertEquals(List("Rub>ble"),m.barney)
    assertEquals("""{"xsType":"Test4","hero":"Flintstone","barney":["Rub>ble"]}""",new String(JSONSerialize.serializeToByteArray(m)))
  }

  def test[T <: AnyRef :ClassTag](expectedXML:String,expectedJSON:String,obj:T) {
    assertEquals(expectedXML,new String(XMLSerialize.serializeToByteArray(obj)))
    val m = XMLDeserialize.deserialize[T](expectedXML.getBytes)
    assertEquals(expectedXML,new String(XMLSerialize.serializeToByteArray(m)))
    assertEquals(expectedJSON,new String(JSONSerialize.serializeToByteArray(m)))
    assertEquals(expectedJSON,new String(JSONSerialize.serializeToByteArray(obj)))
    val m2 = JSONDeserialize.deserialize[T](expectedJSON.getBytes)
    assertEquals(expectedJSON,new String(JSONSerialize.serializeToByteArray(m2)))
    assertEquals(expectedXML,new String(XMLSerialize.serializeToByteArray(m2)))
    val bson = MongoDBSerialize.serializeDBO(obj)
    val m3 = JSONDeserialize.deserialize[T](bson.toString)
    assertEquals(expectedJSON,new String(JSONSerialize.serializeToByteArray(m3)))
    assertEquals(expectedXML,new String(XMLSerialize.serializeToByteArray(m3)))
    val m4 = MongoDBDeserialize.deserialize[T](bson)
    assertEquals(expectedJSON,new String(JSONSerialize.serializeToByteArray(m4)))
    assertEquals(expectedXML,new String(XMLSerialize.serializeToByteArray(m4)))
    val map = StringMapSerialize.serialize(obj)
    val unmap = StringMapDeserialize.deserialize(map)
    assertEquals(map,StringMapSerialize.serialize(unmap))
    assertEquals(expectedXML,new String(XMLSerialize.serializeToByteArray(unmap)))
  }

  @Test
  def test5 {
    val t1 = new Test1("Flintstone",List("Rubble"))
    val t2a = new Test2(4,3.6,Array(5,4,3))
    val t2b = new Test2(0,-1.6,null)
    test("""<T5><Test1 fred="Flintstone" barney="Rubble"/></T5>""","""{"xsType":"T5","f1":{"xsType":"Test1","fred":"Flintstone","barney":["Rubble"]}}""",new Test5(t1,null))
    test("""<T5><Test1 fred="Flintstone" barney="Rubble"/><Test2 f1="4" f2="3.6" f3="5;4;3"/><Test2 f1="0" f2="-1.6"/></T5>""",
         """{"xsType":"T5","f1":{"xsType":"Test1","fred":"Flintstone","barney":["Rubble"]},"f2":[{"xsType":"Test2","f1":4,"f2":3.6,"f3":[5,4,3]},{"xsType":"Test2","f1":0,"f2":-1.6}]}""",
         new Test5(t1,Array(t2a,t2b)))
    test("""<T5><Test1 fred="Flintstone" barney="Rubble"/><Test2 f1="4" f2="3.6" f3="5;4;3"/><null-f2/><Test2 f1="0" f2="-1.6"/></T5>""",
         """{"xsType":"T5","f1":{"xsType":"Test1","fred":"Flintstone","barney":["Rubble"]},"f2":[{"xsType":"Test2","f1":4,"f2":3.6,"f3":[5,4,3]},null,{"xsType":"Test2","f1":0,"f2":-1.6}]}""",
         new Test5(t1,Array(t2a,null,t2b)))
  }
  
  @Test
  def test6 { // test string deserialization using static java valueOf
    test("""<Test6 i="3"/>""","""{"xsType":"Test6","i":3}""",new Test6(new java.lang.Integer(3)))
  }

  @Test
  def test7 { // test string deserialization using scala companion object
    test("""<Test7 f="3"/>""","""{"xsType":"Test7","f":"3"}""",new Test7(new Test7a(3)))
  }

  @Test
  def test8 { // test string deserialization using constructor
    test("""<Test8 f="3"/>""","""{"xsType":"Test8","f":"3"}""",new Test8(new Test8a("3")))
  }
  
  @Test
  def test9 {
     test("""<Test9><Poly1/></Test9>""","""{"xsType":"Test9","p":{"xsType":"Poly1"}}""",new Test9(new Poly1))
     test("""<Test9><P2/></Test9>""","""{"xsType":"Test9","p":{"xsType":"P2"}}""",new Test9(new Poly2))
     test("""<Test9><Poly4/></Test9>""","""{"xsType":"Test9","p":{"xsType":"Poly4"}}""",new Test9(new Poly4))
  }

  @Test
  def test10 {
    shouldFail[Test10Fail]
    test("""<Test10Pass><p1><Poly1/></p1><p2><P2/></p2></Test10Pass>""","""{"xsType":"Test10Pass","p1":{"xsType":"Poly1"},"p2":{"xsType":"P2"}}""",new Test10Pass(new Poly1,new Poly2))
  }
   
  @Test
  def test11 { // test cyclic structures
     test("""<Test11><Test11/></Test11>""","""{"xsType":"Test11","t":[{"xsType":"Test11"}]}""",new Test11(List(new Test11(Nil))))
     test("""<Test11><Test11/><Test11><Test11/></Test11></Test11>""",
          """{"xsType":"Test11","t":[{"xsType":"Test11"},{"xsType":"Test11","t":[{"xsType":"Test11"}]}]}""",
          new Test11(List(new Test11(Nil),new Test11(List(new Test11(Nil))))))
  }

  @Test
  def test12 { // test optional structures
     test("""<Test12><Test12/></Test12>""","""{"xsType":"Test12","t":{"xsType":"Test12"}}""",new Test12(Some(new Test12(None))))
  }
  
  @Test
  def testA1 { // test arrays of null as an attribute
    test("""<TestA1 a="5;;4"/>""","""{"xsType":"TestA1","a":[5,null,4]}""",new TestA1(Array(5,null,4)))
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
     test("""<Test13 t1="fr;ed"><t2>bar;ney</t2></Test13>""","""{"xsType":"Test13","t1":"fr;ed","t2":"bar;ney"}""",new Test13(Some("fr;ed"),Some("bar;ney")))
     test("""<Test13/>""","""{"xsType":"Test13"}""",new Test13(None,None))
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


