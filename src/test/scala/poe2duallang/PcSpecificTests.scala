package poe2duallang

import cats.data.Validated
import org.scalatest.{FlatSpec, Matchers}

final class PcSpecificTests extends FlatSpec with Matchers {

  val pillarsRoot = "D:\\Games\\Steam\\steamapps\\common\\Pillars of Eternity II\\"


  "main program" should "work" in {
    val args = Array(s"gameDir=$pillarsRoot", "entryLang=en", "targetLang=de")
    Main.program(args) should not be a[Validated.Invalid[_]]
  }


}
