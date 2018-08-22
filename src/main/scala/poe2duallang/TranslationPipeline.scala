package poe2duallang

import java.io.File

import cats.effect.IO
import poe2duallang.PoeSpecific.GameData
import poe2duallang.utils.{FileUtilsIO, XmlUtils}

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

    val xmlTrans = translateTranslationsXml(newLang.langXml, translations)

    val filesTrans = {
      newLang.listFiles.filterNot(_.isDirectory).map { newLangFile =>
        val newLangPath = newLangFile.toPath
        val relative = transPath.relativize(newLangPath)
        translateFile(newLangFile, new File(translations.pathTo.toString, relative.toString))
      }.sequence_
    }

    xmlTrans >> filesTrans
  }

  private def translateTranslationsXml(translationXml:File, translations:Poe2Localized):IO[Unit] = IO {
    val (translatedName, translatedGuiString) = {
      val xml = XML.loadFile(translations.langXml)
      val language = xml.head
      val name = XmlUtils.collectFirst(language, "Name").get
      val gui = XmlUtils.collectFirst(language, "GUIString").get
      (name, gui)
    }

    val run = new RewriteRule {
      override def transform(n:Node): Seq[Node] = {
        n match {
          case elem: Elem if elem.label == "Name" =>
            elem.copy(child = elem.child collect {
              case Text(data) => Text(s"$data-to-$translatedName")
            })

          case elem: Elem if elem.label == "GUIString" =>
            elem.copy(child = elem.child collect {
              case Text(data) => Text(s"$data-to-$translatedGuiString")
            })

          case elem: Elem => elem.copy(child = elem.child.flatMap(transform))
          case n => n
        }
      }
    }

    val langFile = XML.loadFile(translationXml)
    val resultXml = run(langFile)
    XML.save(filename = translationXml.getAbsolutePath,
      node = resultXml,
      xmlDecl = true)
  }

  private def translateFile(newLangFile:File, target:File):IO[Unit] = IO {

    def collectId(n:Node):Option[String] = XmlUtils.collectFirst(n, "ID")
    def collectText(n:Node):Option[String] = XmlUtils.collectFirst(n, "DefaultText")

    val newLangXml = XML.loadFile(newLangFile)
    val targetTranslations:Map[String, String] = {
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
      def shouldTranslate(s:String):Boolean = s.exists(_.isLetter)

      def translateFmt(from:String, trans:String):String = {
        val quotation = "\""

        def quoteTrans(s:String) = s"«$s»"
        def sanitize(s:String):String = s.replaceAll(s"$quotation$quotation", quotation)

/*      disabled until figured out
        val (fromSplit, toSplit) = (sanitize(from).split(quotation), sanitize(trans).split(quotation))
        // format is: `"someone said" some action`
        val t = if (fromSplit.size == 3 && toSplit.size == 3) {
          val fst = s"${fromSplit(1)} ${quoteTrans(toSplit(1))}"
          val snd = s"${fromSplit(2)} ${quoteTrans(toSplit(2))}"
          s"${quotation}$fst${quotation}$snd"
        } else {
          s"$from ${quoteTrans(trans)}"
        }*/

        s"$from ${quoteTrans(trans)}"
      }

      override def transform(n:Node): Seq[Node] = {
        n match {
          case elem: Elem if elem.label == "DefaultText" =>
            elem.copy(child = elem.child.collect {
              case Text(data) if shouldTranslate(data) =>
                val trans = targetTranslations.get(entryId)
                val text = trans match {
                  case Some(t) if t.trim == data.trim => data
                  case Some(t) => translateFmt(data, t)
                  case None =>
                    println(s"Not found translation for key=$entryId, text=$data")
                    s"$data «?»"
                }
                Text(text)

              case t@Text(_) => t
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
