package poe2duallang

import java.io.File

import cats.data.{Validated, ValidatedNel}
import poe2duallang.ArgsParser.ProgramArgs
import cats.implicits._

import scala.util.Try

object PoeSpecific {
  type TargetDir = File

  final case class GameData(gameDirectory:Poe2GameRoot, entryLang:Poe2Localized, targetLang:Poe2Localized)

  def create(programArgs:ProgramArgs):ValidatedNel[String, GameData] = {
    val gameRoot = Poe2GameRoot.open(programArgs.gameDir)
    gameRoot.andThen { game =>
      val entry = game.openLanguage(programArgs.entryLang)
      val target = game.openLanguage(programArgs.targetLang)

      (entry, target).mapN { (e, t) => GameData(game, e, t) }
    }
  }

}
