package scalaprops

import scalaz._
import scalaz.std.tuple._
import scalaz.std.anyVal._

object FunTest extends Scalaprops {

  type KEY = Boolean //(Boolean, Boolean)
  type VAL = List[Byte]

  val x = collection.concurrent.TrieMap.empty[IMap[KEY, VAL], Unit]
  val test = Property.forAll{ f: Func[KEY, VAL] =>
    println(f.table.map(_._2.size).toList)
    true
  }
}
