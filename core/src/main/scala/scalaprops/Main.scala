package scalaprops

object Main {

  def main(args: Array[String]): Unit = {
    val values = Gen[Long => Long].samples(listSize = 10).map{f => (1L to 5).map(f)}
    println(values.size)
    values foreach println
  }
}
