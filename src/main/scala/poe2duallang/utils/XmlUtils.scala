package poe2duallang.utils

import scala.xml._

object XmlUtils {

  def collectFirst(n:Node, label:String):Option[String] = {
    n.child.collectFirst { case e: Elem if e.label == label => e.child.collectFirst { case Text(id) => id } }.flatten
  }

}
