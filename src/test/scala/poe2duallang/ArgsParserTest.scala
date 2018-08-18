package poe2duallang

import cats.data.{NonEmptyList, Validated}
import org.scalatest.{FlatSpec, Matchers}
import ArgsParser.ProgramArgs

final class ArgsParserTest extends FlatSpec with Matchers {

  "arguments parser" should "parse correct" in {
    val args = NonEmptyList.of("gameDir=C:/games", "entryLang=en", "targetLang=de")
    ArgsParser.parse(args) shouldBe Validated.valid(ProgramArgs("C:/games", "en", "de"))
  }

  "arguments parser" should "not parse incorrect empty" in {
    val args = NonEmptyList.of("gameDir=", "entryLang=en", "targetLang=de")
    ArgsParser.parse(args) shouldBe a[Validated.Invalid[_]]
  }

  "arguments parser" should "not parse incorrect" in {
    val args = NonEmptyList.of("gameDir=C:/games", "entryLan=en", "targetLang=de")
    ArgsParser.parse(args) shouldBe a[Validated.Invalid[_]]
  }

  "arguments parser" should "not parse incorrect2" in {
    val args = NonEmptyList.of("foo")
    ArgsParser.parse(args) shouldBe a[Validated.Invalid[_]]
  }
}
