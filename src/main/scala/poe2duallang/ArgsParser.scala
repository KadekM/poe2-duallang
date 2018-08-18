package poe2duallang

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._

object ArgsParser {
  final case class ProgramArgs(gameDir:String, entryLang:String, targetLang:String)

  val gameDirArgKey="gameDir"
  val entryLangArgKey="entryLang"
  val targetLangArgKey="targetLang"

  val keys:NonEmptyList[String] = NonEmptyList.of(gameDirArgKey, entryLangArgKey, targetLangArgKey)

  def parse(args:NonEmptyList[String]):ValidatedNel[String, ProgramArgs] = {

    if (args.size != keys.size) {
      Validated.invalidNel(s"${Texts.invalidArgumentCount} ${Texts.usage}")
    } else {
      val kv:Validated[String, NonEmptyList[(String, String)]] = args.map { arg =>
        val arr = arg.split("=").toList
        arr match {
          case k :: _ :: Nil if !keys.exists(_ == k)  => Validated.invalid(s"Key $k is meaningles")
          case k :: v :: Nil if v.isEmpty  => Validated.invalid(s"Key $k value $v is empty")
          case k :: v :: Nil => Validated.valid((k, v))
          case _ => Validated.invalid(s"Invalid format of key: $arg")
        }
      }.sequence

      val asMap = kv.leftMap(NonEmptyList.of(_)).map(_.toList.toMap)
      asMap.andThen { m =>
        val a1 = m.get(gameDirArgKey).toValidNel(s"Missing $gameDirArgKey!")
        val a2 = m.get(entryLangArgKey).toValidNel(s"Missing $entryLangArgKey!")
        val a3 = m.get(targetLangArgKey).toValidNel(s"Missing $targetLangArgKey!")

        (a1, a2, a3).mapN(ProgramArgs)
      }
    }
  }
}
