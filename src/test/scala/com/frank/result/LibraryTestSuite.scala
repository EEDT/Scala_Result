/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
package com.frank.result

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LibraryTestSuite extends AnyFunSuite {
  test("Map"){
    val y = for(i <- Ok(10)) yield i + 1
    Ok(100).foreach(println)
    assert(y.unwrap == 11)
  }
}
