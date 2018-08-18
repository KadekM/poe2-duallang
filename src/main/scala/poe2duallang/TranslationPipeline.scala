package poe2duallang

import java.io.File

import cats.effect.IO
import poe2duallang.PoeSpecific.GameData
import poe2duallang.utils.FileUtilsIO

import scala.xml._
import cats.implicits._

import scala.xml.transform.RewriteRule

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

    def collectId(n:Node):Option[String] = n.child.collectFirst { case e: Elem if e.label == "ID" => e.child.collectFirst { case Text(id) => id } }.flatten
    def collectText(n:Node):Option[String] = n.child.collectFirst { case e: Elem if e.label == "DefaultText" => e.child.collectFirst { case Text(text) => text } }.flatten

    val newLangXml = XML.loadFile(newLangFile)
    val targetTranslations = {
      XML.loadFile(target).descendant.collect {
        case elem: Elem if elem.label == "Entry" =>
          val id = collectId(elem)
          val trans = collectText(elem)

          (id, trans).mapN { (k, v) => k -> v }
      }
        .collect { case Some(x) => x }
        .toMap
    }

    def textTrans(entryId:String) = new RewriteRule {
      override def transform(n:Node): Seq[Node] = {
        n match {
          case elem: Elem if elem.label == "DefaultText" =>
            elem.copy(child = elem.child collect {
              case Text(data) =>
                val removeQuotes = data.drop(1).dropRight(1)
                val trans = targetTranslations(entryId) // todo: unsafe
                val translated = s"$removeQuotes «$trans»"
                val withQuotes = s""""$translated""""
                Text(withQuotes)
            })
          case elem: Elem => elem.copy(child = elem.child.flatMap(transform))
          case n => n
        }
      }
    }

    val translateRule = new RewriteRule {
      override def transform(n:Node): Seq[Node] = {
        n match {
          case elem: Elem if elem.label == "Entry" =>
            collectId(elem) match {
              case Some(id) =>
                elem.copy(child = elem.child.flatMap(textTrans(id)))
              case None =>
                println(s"Failed to translate $elem")
                elem
            }
          case elem: Elem => elem.copy(child = elem.child.flatMap(transform))
          case n => n
        }
      }
    }

    val resultXml = translateRule(newLangXml)
    XML.save(filename = newLangFile.getAbsolutePath,
      node = resultXml,
      xmlDecl = true)
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
