package scalaprops

import scalaz._

object FunTest extends Scalaprops {

  type KEY = List[Byte]
  type VAL = List[Byte]

  val x = collection.concurrent.TrieMap.empty[IMap[KEY, VAL], Unit]
  val test = Property.forAll{ (f: Fun[KEY, VAL], b: Byte) =>
    println(f.fun(List(b)).size)
    true
  }
}
