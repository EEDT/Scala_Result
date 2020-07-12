/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
package com.frank.result
import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LibraryTestSuite extends AnyFunSuite {
  test("exists"){
    assert(Ok(10).exists(_ == 10))
  }
  test("map"){
    val x = Ok(10).map(_ + 1)
    assert(x.contains(11))
  }
  test("contains"){
    val x = Ok(10)
    assert(x.contains(10))
  }
  test("containsErr"){
    val testCase = Err(new NullPointerException)
    assert(testCase.containsErr(new NullPointerException))
  }
  test("flatMap"){
    val testSuite = Ok(10)
    val testCase = testSuite.flatMap(x => Vector(x,x+1))
    assert(testCase == Seq(10,11))
  }
  test("foreach"){
    var i = 0
    Ok(100).foreach(x => {
      i += x
    })
    assert(i == 100)
  }
  test("unwrap"){
    def toInt(x:String):Result[Int,NumberFormatException] =
      try Ok(x.toInt) catch {
      case ex:NumberFormatException => Err(ex)
    }
    assert(toInt("10").unwrap == 10)
  }
  test("exception"){
    assertThrows[RuntimeException](Err("str").exception("error"))
  }
  test("isOk"){
    assert(Ok(10).isOK)
  }
  test("isErr"){
    assert(Err(10).isErr)
  }
  test("err"){
    assert(Err(20).err.contains(20))
  }
  test("ok"){
    assert(Ok(20).ok.contains(20))
  }
  test("ok_else"){
    assert(Ok(20).okOrElse(0) == 20)
  }
}
