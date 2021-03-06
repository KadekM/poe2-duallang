package poe2duallang

import ArgsParser._
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._


object Main {
  import Texts._

  def program(args:Array[String]):ValidatedNel[String, _] = {
    NonEmptyList.fromList(args.toList).toValidNel(usage)
      .andThen(ArgsParser.parse)
      .andThen(PoeSpecific.create)
      .andThen { g =>
        TranslationPipeline.translate(g)
          .attempt
          .unsafeRunSync()
          .toValidatedNel
          .leftMap[NonEmptyList[String]](x => x.map(_.getMessage))
      }
  }

  def main(args:Array[String]):Unit = {
    program(args) match {
      case Validated.Invalid(e) => System.err.println(reportErrs(e))
      case Validated.Valid(_) => System.out.println("done")
    }
  }

}


