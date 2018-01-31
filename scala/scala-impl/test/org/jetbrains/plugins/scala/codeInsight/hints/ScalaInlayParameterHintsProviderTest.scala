package org.jetbrains.plugins.scala
package codeInsight
package hints

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.junit.Assert.{assertFalse, assertTrue}

class ScalaInlayParameterHintsProviderTest extends ScalaLightCodeInsightFixtureTestAdapter {

  import ScalaInlayParameterHintsProvider._
  import ScalaInlayParameterHintsProviderTest.{HintEnd => E, HintStart => S, _}

  def testParameterHint(): Unit = doParameterTest(
    s"""  def foo(foo: Int, otherFoo: Int = 42)
       |         (bar: Int)
       |         (baz: Int = 0): Unit = {}
       |
       |  foo(${S}foo =${E}42, ${S}otherFoo =${E}42)(${S}bar =${E}42)()
       |  foo(${S}foo =${E}42)(bar = 42)()
       |  foo(${S}foo =${E}42, ${S}otherFoo =${E}42)(${S}bar =${E}42)(${S}baz =${E}42)""".stripMargin
  )

  def testNoInfixExpressionHint(): Unit = doParameterTest(
    s"""  def foo(foo: Int): Unit = {}
       |
       |  this foo 42""".stripMargin
  )

  def testVarargHint(): Unit = doParameterTest(
    s"""  def foo(int: Int, ints: Int*): Unit = {}
       |
       |  foo(${S}int =${E}42)
       |  foo(${S}int =${E}42, ints = 42, 42 + 0)
       |  foo(int = 42)
       |  foo(int = 42, ${S}ints =${E}42, 42 + 0)
       |  foo(${S}int =${E}42, ${S}ints =${E}42, 42 + 0)
       |  foo(int = 42, ints = 42, 42 + 0)""".stripMargin
  )

  def testFunctionReturnTypeHint(): Unit = doTest(
    s"""  def foo()$S: List[String]$E = List.empty[String]"""
  )(hintType = ReturnTypeHintType)

  def testNoFunctionReturnTypeHint(): Unit = doTest(
    """  def foo(): List[String] = List.empty[String]"""
  )(hintType = ReturnTypeHintType)

  def testPropertyTypeHint(): Unit = doTest(
    s"""  val list$S: List[String]$E = List.empty[String]"""
  )(hintType = PropertyHintType)

  def testNoPropertyTypeHint(): Unit = doTest(
    """  val list: List[String] = List.empty[String]"""
  )(hintType = PropertyHintType)

  def testLocalVariableTypeHint(): Unit = doTest(
    s"""  def foo(): Unit = {
       |    val list$S: List[String]$E = List.empty[String]
       |  }""".stripMargin
  )(hintType = LocalVariableHintType)

  def testNoLocalVariableTypeHint(): Unit = doTest(
    s"""  def foo(): Unit = {
       |    val list: List[String] = List.empty[String]
       |  }""".stripMargin
  )(hintType = LocalVariableHintType)

  private def doTest(text: String)
                    (hintType: HintType): Unit = {
    hintType match {
      case null =>
      case _ =>
        import hintType._
        assertFalse(isOptionEnabled)
        enable()
        assertTrue(isOptionEnabled)
    }

    getFixture.configureByText(ScalaFileType.INSTANCE, createFileText(text))
    getFixture.testInlays()

    hintType match {
      case null =>
      case _ =>
        import hintType._
        //    assertTrue(isOptionEnabled) // TODO ???
        disable()
        assertFalse(isOptionEnabled)
    }
  }

  private def doParameterTest(text: String): Unit =
    doTest(text)(hintType = null)

}

object ScalaInlayParameterHintsProviderTest {

  private val HintStart = "<hint text=\""
  private val HintEnd = "\" />"

  private def createFileText(text: String) =
    ScalaLightCodeInsightFixtureTestAdapter.normalize(
      s"""class Foo {
         |$text
         |}
         |
         |new Foo""".stripMargin
    )
}
