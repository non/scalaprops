package scalaprops

import scalaz._
import scalaz.std.tuple._
import scalaz.std.anyVal._

object FunTest extends Scalaprops {

  type KEY = (Boolean, Boolean)
  type VAL = Boolean

  val x = collection.concurrent.TrieMap.empty[IMap[KEY, VAL], Unit]
  val test = Property.forAll{ f: Func[KEY, VAL] =>
    val s = x.size
    x += ((f.table.toMap[KEY, VAL], ()))
    if(s != x.size){
      println((f, s))
    }
    true
  }
}
