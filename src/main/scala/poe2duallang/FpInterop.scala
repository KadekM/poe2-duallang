package poe2duallang

import java.io.{File, FileInputStream, FileOutputStream}

import cats.effect.IO
import poe2duallang.PoeSpecific.TargetDir
import cats.implicits._

import scala.util.Try

// should be IO but meh
object FpInterop {
 def safeFile(path:File):Try[TargetDir] = safeFile(path.getAbsolutePath)

 def safeFile(path:String):Try[TargetDir] = Try {
  val f = new File(path)
  if (!f.exists()) throw new Error(s"File on $path does not exist")
  f
 }
}

object FpInteropIO {
 def copy(src:File, target:File): IO[Unit] = IO {
  new FileOutputStream(target).getChannel.transferFrom(
   new FileInputStream(src).getChannel, 0, Long.MaxValue)
 }
}
