package poe2duallang

import java.io.File

import cats.data.ValidatedNel
import cats.implicits._
import FpInterop._

final case class Poe2GameRoot(pathTo:File) {
  def openLanguage(language: String): ValidatedNel[String, Poe2Localized] = {
    val folderPath = List(pathTo.getAbsolutePath, "PillarsOfEternityII_Data", "exported", "localized", language).mkString(File.separator)
    val folder = safeFile(folderPath).fold(_ => Left(s"Language $language not found (on $folderPath)"), x => Right(x)).toValidatedNel

    val langXmlPath = List(folderPath, "language.xml").mkString(File.separator)
    val langXml = safeFile(langXmlPath).fold(_ => Left(s"Language xml $language not found (on $langXmlPath)"), x => Right(x)).toValidatedNel

    (folder, langXml).mapN(Poe2Localized)
  }
}
object Poe2GameRoot {
  def open(path:String):ValidatedNel[String, Poe2GameRoot] = {
    safeFile(path).fold(_ => Left("Game directory not found"), x => Right(Poe2GameRoot(x))).toValidatedNel
  }
}

final case class Poe2Localized(pathTo:File, langXml:File)

