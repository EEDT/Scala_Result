/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
package com.frank.result

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LibrarySuite extends AnyFunSuite {
  test("Main"){
    for(i <- Ok(123)) println(i)
    assert(Ok(123).Ok.getOrElse(1) == 123)
  }
  test("err"){
    assertThrows[RuntimeException](Err("error").exception("error"))
  }
}
