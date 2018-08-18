package poe2duallang

import java.io.File

import cats.data.ValidatedNel
import cats.implicits._
import FpInterop._

final case class Poe2GameRoot(pathTo:File) {

  def openLanguage(language:String): ValidatedNel[String, Poe2Localized] = {
    val folderPath = localizedFolder(language)
    val folder = safeFile(folderPath).fold(_ => Left(s"Language $language not found (on $folderPath)"), x => Right(x)).toValidatedNel

    val langXmlPath = folderPath / "language.xml"
    val langXml = safeFile(langXmlPath).fold(_ => Left(s"Language.xml for $language not found (on $langXmlPath)"), x => Right(x)).toValidatedNel

    val textPath = folderPath / "text"
    val text = safeFile(textPath).fold(_ => Left(s"Folder text for $language not found (on $textPath)"), x => Right(x)).toValidatedNel

    (folder, langXml, text).mapN(Poe2Localized)
  }

  def localizedFolder:File = {
    pathTo / "PillarsOfEternityII_Data" / "exported" / "localized"
  }

  def localizedFolder(language:String):File = {
    pathTo / "PillarsOfEternityII_Data" / "exported" / "localized" / language
  }
}
object Poe2GameRoot {
  def open(path:String):ValidatedNel[String, Poe2GameRoot] = {
    safeFile(path).fold(_ => Left("Game directory not found"), x => Right(Poe2GameRoot(x))).toValidatedNel
  }
}

final case class Poe2Localized(pathTo:File, langXml:File, text:File) {
  def listFiles:List[File] = utils.FileUtils.getFileTree(text).toList
}

