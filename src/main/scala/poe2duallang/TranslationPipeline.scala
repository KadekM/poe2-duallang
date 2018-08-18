package poe2duallang

import java.io.File

import cats.effect.IO
import poe2duallang.PoeSpecific.GameData
import poe2duallang.utils.FileUtilsIO

import cats.implicits._

object TranslationPipeline {

  def translate(data:GameData):IO[Unit] = {
    for {
      newLangData <- createStructure(data)
      _ <- trans(newLangData, data.targetLang, data.gameDirectory.localizedFolder)
    } yield()
  }

  private def trans(newLang:Poe2Localized, translations:Poe2Localized, localRootDir:File):IO[Unit] = {
    val rootPath = localRootDir.toPath
    val transPath = newLang.pathTo.toPath

    val xs = newLang.listFiles.filterNot(_.isDirectory).map { newLangFile =>
      val newLangPath = newLangFile.toPath
      val relative = transPath.relativize(newLangPath)
      translateFile(newLangFile, new File(translations.pathTo.toString, relative.toString))
    }
    xs.sequence_
  }

  private def translateFile(newLangFile:File, target:File):IO[Unit] = IO {
    println(newLangFile.getAbsolutePath)
    println(target.getAbsolutePath)
    ???
  }

  private def createStructure(data:GameData):IO[Poe2Localized] = {
    val lang = s"${data.entryLang.pathTo.getName}-${data.targetLang.pathTo.getName}"
    val transFolder = data.gameDirectory.localizedFolder(lang)

    val delete = if (transFolder.exists) FileUtilsIO.deleteRec(transFolder)
    else IO.pure(false)

    val creation = IO {transFolder.mkdir() }
    val copy = FileUtilsIO.copy(data.entryLang.pathTo, transFolder)
    val langOpen = IO { data.gameDirectory.openLanguage(lang).fold(e => throw new Exception(e.toList.mkString(",")), identity) }
    delete >> creation >> copy >> langOpen
  }
}
