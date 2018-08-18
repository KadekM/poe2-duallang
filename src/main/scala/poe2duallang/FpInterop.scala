package poe2duallang

import java.io.File

import poe2duallang.PoeSpecific.TargetDir

import cats.implicits._
import scala.util.Try

object FpInterop {
 def safeFile(path:String):Try[TargetDir] = Try {
  val f = new File(path)
  if (!f.exists()) throw new Error(s"File on $path does not exist")
  f
 }
}
