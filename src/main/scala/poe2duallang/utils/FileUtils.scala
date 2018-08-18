package poe2duallang.utils

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

object FileUtils {
  def getFileTree(f:File):Stream[File] =
    f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree)
    else Stream.empty)
}

object FileUtilsIO {
  import cats.effect.IO

  def copy(src:File, target:File):IO[Unit] = IO {
    Files.walkFileTree(src.toPath, new UnsafeCopyFileVisitor(target.toPath))
  }

  def deleteRec(dir:File) = {
    def go(file: File): Unit = {
      if (file.isDirectory) file.listFiles.foreach(go)
      if (file.exists && !file.delete) throw new Exception(s"Unable to delete ${file.getAbsolutePath}")
    }
    IO {go(dir)}
  }
}

private final class UnsafeCopyFileVisitor(target:Path) extends SimpleFileVisitor[Path] {
  private var sourcePath:Option[Path] = None

  override def preVisitDirectory(dir:Path, attrs:BasicFileAttributes):FileVisitResult = {
    sourcePath match {
      case None => sourcePath = Some(dir)
      case Some(p) =>
        Files.createDirectories(target.resolve(p.relativize(dir)))
    }
    FileVisitResult.CONTINUE
  }

  override def visitFile(file:Path, attrs:BasicFileAttributes):FileVisitResult = {
    sourcePath match {
      case None => FileVisitResult.CONTINUE
      case Some(p) =>
        val resolved = target.resolve(p.relativize(file))
        Files.copy(file, resolved)
        FileVisitResult.CONTINUE
    }
  }
}
