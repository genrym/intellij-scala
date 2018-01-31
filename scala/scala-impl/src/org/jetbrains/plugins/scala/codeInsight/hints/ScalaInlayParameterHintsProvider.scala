package org.jetbrains.plugins.scala
package codeInsight
package hints

import java.{util => ju}

import com.intellij.codeInsight.hints.{Option => HintOption, _}
import com.intellij.lang.java.JavaLanguage
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.ScPatternList
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.Parameter

import scala.collection.JavaConverters

class ScalaInlayParameterHintsProvider extends InlayParameterHintsProvider {

  import ScalaInlayParameterHintsProvider._
  import JavaConverters._

  override def getSupportedOptions: ju.List[HintOption] =
    HintTypes.map(_.option).asJava

  override def getParameterHints(element: PsiElement): ju.List[InlayInfo] =
    parameterHints(element).asJava

  override def getHintInfo(element: PsiElement): HintInfo =
    hintInfo(element).orNull

  override def getInlayPresentation(inlayText: String): String =
    InlayInfo.presentation(inlayText)

  override def getDefaultBlackList: ju.Set[String] =
    DefaultBlackList.asJava

  override def getBlackListDependencyLanguage: JavaLanguage = JavaLanguage.INSTANCE

  override def canShowHintsWhenDisabled: Boolean = true
}

object ScalaInlayParameterHintsProvider {

  private val DefaultBlackList = Set.empty[String]

  private val HintTypes = List(
    ParameterHintType,
    ReturnTypeHintType,
    PropertyHintType,
    LocalVariableHintType
  )

  def instance: ScalaInlayParameterHintsProvider =
    InlayParameterHintsExtension.INSTANCE.forLanguage(ScalaLanguage.INSTANCE) match {
      case provider: ScalaInlayParameterHintsProvider => provider
    }

  private def parameterHints(element: PsiElement): Seq[InlayInfo] =
    HintTypes.flatMap { hintType =>
      hintType(element)
    }

  private def hintInfo(element: PsiElement): Option[HintInfo] =
    HintTypes.find(_.isDefinedAt(element))

  private[this] type HintFunction = PartialFunction[PsiElement, Seq[InlayInfo]]

  private[hints] sealed abstract class HintType protected(defaultValue: Boolean, idSegments: String*)
    extends HintInfo.OptionInfo(HintOption(idSegments, defaultValue)) with HintFunction {

    private[hints] def option = getOption

    override def enable(): Unit =
      if (!option.get()) {
        option.set(true)
      }

    override def disable(): Unit =
      if (option.get()) {
        option.set(false)
      }

    protected val delegate: HintFunction

    override final def isDefinedAt(element: PsiElement): Boolean = delegate.isDefinedAt(element)

    override final def apply(element: PsiElement): Seq[InlayInfo] =
      if (isOptionEnabled && isDefinedAt(element)) delegate(element)
      else Seq.empty
  }

  private[this] object HintOption {

    def apply(idSegments: Seq[String], defaultValue: Boolean): HintOption = {
      val id = "scala" +: idSegments :+ "hint"
      new HintOption(id.mkString("."), s"Show ${idSegments.mkString(" ")} hints", defaultValue)
    }
  }

  private[hints] case object ParameterHintType extends HintType(defaultValue = true, "parameter", "name") {

    override protected val delegate: HintFunction = {
      case call: ScMethodCall =>
        // TODO functional scopes
        val (varargs, regular) = call.matchedParameters.filter {
          case (argument, _) => call.isAncestorOf(argument)
        }.partition {
          case (_, parameter) => parameter.isRepeated
        }

        (regular ++ varargs.lastOption).collect {
          case (argument, parameter) if !isNamed(argument) => InlayInfo(parameter.name, argument)
        }
    }

    private def isNamed(argument: ScExpression) =
      argument.getParent.isInstanceOf[ScAssignStmt]
  }

  private[hints] case object ReturnTypeHintType extends HintType(defaultValue = false, "function", "return", "type") {

    override protected val delegate: HintFunction = {
      case function: ScFunction if !function.hasExplicitType =>
        function.returnType.toSeq
          .map(InlayInfo(_, function.parameterList))
    }
  }

  private[hints] abstract class DefinitionHintType(isLocal: Boolean, idSegments: String*)
    extends HintType(defaultValue = false, idSegments :+ "type": _*) {

    import DefinitionHintType._

    override protected val delegate: HintFunction = {
      case TypelessDefinition(definition, patternList, `isLocal`) =>
        definition.`type`().toSeq
          .map(InlayInfo(_, patternList))
    }
  }

  private[this] object DefinitionHintType {

    private object TypelessDefinition {

      def unapply(element: PsiElement): Option[(ScValueOrVariable, ScPatternList, Boolean)] = element match {
        case definition: ScValueOrVariable if !definition.hasExplicitType =>
          val maybePatternList = definition match {
            case value: ScPatternDefinition => Some(value.pList)
            case variable: ScVariableDefinition => Some(variable.pList)
            case _ => None
          }

          maybePatternList.map((definition, _, definition.isLocal))
        case _ => None
      }
    }

  }

  private[hints] case object PropertyHintType extends DefinitionHintType(isLocal = false, "property")

  private[hints] case object LocalVariableHintType extends DefinitionHintType(isLocal = true, "local", "variable")

  private object InlayInfo {

    private[this] val TypeInfoPrefix = "@TYPE@"

    def apply(text: String, anchor: PsiElement, isParameter: Boolean = true): InlayInfo = {
      val textRange = anchor.getTextRange
      val offset = if (isParameter) textRange.getStartOffset else textRange.getEndOffset
      new InlayInfo(text, offset)
    }

    def apply(`type`: ScType, anchor: PsiElement): InlayInfo =
      apply(TypeInfoPrefix + `type`.presentableText, anchor, isParameter = false)

    def presentation(text: String): String = {
      import ScalaTokenTypes.{tCOLON, tASSIGN}
      text.stripPrefix(TypeInfoPrefix) match {
        case `text` => s"$text $tASSIGN"
        case strippedText => s"$tCOLON $strippedText"
      }
    }

  }

}