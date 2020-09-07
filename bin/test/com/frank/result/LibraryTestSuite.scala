package com.frank.result

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.diagrams._
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import scala.util.{Failure, Success, Try}
import Result.{either2Result, fromEither, result2Either}

@RunWith(classOf[JUnitRunner])
class LibraryTestSuite extends AnyFunSuite with Diagrams {
  test("exists"){
    assert(Ok(10).exists(_ == 10))
  }
  test("map"){
    val x = Ok(10).map((_:Int) + 1)
    assert(x.contains(11))
  }
  test("map2"){
    val ex = Err[Int,Int](100).map((_:Int) + 1)
    assert(ex == Err(100))
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
    assert(Ok(10).unwrap == 10)
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
  test("unwrap err"){
    assert(Err(10).unwrapErr == 10)
  }
  test("except err"){
    assertThrows[RuntimeException](Ok(10).exceptionErr("error"))
  }
  test("unwrap or default"){
    def toInteger(s:String):Result[Int,NumberFormatException] = try Ok(s.toInt) catch {
      case ex:NumberFormatException => Err(ex)
    }
    val result = toInteger("123a").unwrapOrDefault(123)
    assert(result == 123)
  }
  test("or_else") {
    val result = Err[Int, Int](10)
    assert(result.unwrapOrElse(_ => 10) == 10)
  }
  test("toSeq"){
    val result = Ok(20)
    assert(result.toSeq == Seq(20))
  }
  test("toSeq2"){
    val result = Err(25)
    assert(result.toSeq == Seq())
  }
  test("map_or_else"){
    def getValue(s:Any):Result[Int,String] = s match {
      case x:Int => Ok(x)
      case x:String => Err(x)
      case _ => throw new IllegalArgumentException("not int or string")
    }
    assert(
      getValue(100).mapOrElse(_+1,_.toUpperCase).contains(101) &&
      getValue("string").mapOrElse(_+1,_.toUpperCase()).containsErr("STRING")
    )
  }
  test("fromEither"){
    assert(
      Result.fromEither(Left(10)).containsErr(10) &&
      Result.fromEither(Right(10)).contains(10)
    )
  }
  test("toEither"){
    assert(Ok(20).toEither == Right(20))
  }
  test("implicits"){
    def parseInt(x:String): Either[Throwable, Int] = Try(x.toInt) match {
      case Failure(exception) =>  Left(exception)
      case Success(value)     =>  Right(value)
    }
    val x = parseInt("123").mapOrElse(x => x,_ => 0).unwrap
    assertResult(123)(x)
  }
  test("implicits2"){
    val testSuite = Ok(20)
    val testSuite2 = fromEither(testSuite)
    assert(testSuite2 == Ok(20))
  }
  test("flatMapOrElse"){
    val testSuiteCreator: String => Result[Int, String] = (x:String) =>
      try Ok(x.toInt)
      catch {
        case _:Exception => Err(x)
      }
    val s1 = testSuiteCreator("123")
    val s2 = testSuiteCreator("bad string")
    val t1 = s1.flatMapOrElse(x => Seq(x),_ => Seq(0)) == Seq(123)
    val t2 = s2.flatMapOrElse(x => Seq(x),_ => Seq(0)) == Seq(0)
    assert(t1 && t2)
  }
}
