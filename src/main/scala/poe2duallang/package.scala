import java.io.File

package object poe2duallang {
  implicit class FilePimper(private val f:File) extends AnyVal {
    def /(s:String):File = {
      val newPath = List(f.getAbsolutePath, s).mkString(File.separator)
      new File(newPath)
    }
  }
}
