package poe2duallang

import cats.data.NonEmptyList

object Texts {

  def reportErrs(errs: NonEmptyList[String]):String =
    s"""Following errors were detected: ${errs.toList.mkString(",")}"""

  val invalidArgumentCount = s"Invalid argument count!"

  val usage:String =
    s"""
       | Provide arguments ${ArgsParser.gameDirArgKey}, ${ArgsParser.entryLangArgKey}, ${ArgsParser.targetLangArgKey}!
       | For example:
       | ${ArgsParser.gameDirArgKey}="D:\\Games\\Steam\\steamapps\\common\\Pillars of Eternity II"
       | ${ArgsParser.entryLangArgKey}=en
       | ${ArgsParser.targetLangArgKey}=de
       """.stripMargin
}
