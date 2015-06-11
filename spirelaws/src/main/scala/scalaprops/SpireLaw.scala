package scalaprops

final class SpireLaw private(val ord: Int, val fullName: String, val simpleName: String) {
  override def hashCode = ord
  override def toString = simpleName
}

object SpireLaw {
  private[this] val set = collection.mutable.Set.empty[SpireLaw]

  private[this] def law(fullName: String, simpleName: String = ""): SpireLaw =
    set.synchronized {
      val name = if (simpleName == "") fullName else simpleName
      val law = new SpireLaw(set.size, fullName, name)
      set += law
      law
    }

  private[this] def law0(clazz: SpireLaw, lawName: String): SpireLaw =
    law(clazz.simpleName + " " + lawName, lawName)

  private[this] def all(clazz: SpireLaw): SpireLaw =
    law(clazz.simpleName+ " all", clazz.simpleName)

  val equal = law("equal")
  val equalSymmetry = law0(equal, "symmetry")
  val equalReflexive = law0(equal, "reflexive")
  val equalTransitive = law0(equal, "transitive")
  val equalNaturality = law0(equal, "naturality")

  val order = law("order")
  val orderAll = all(order)
  val orderAntisymmetric = law0(order, "anti symmetric")
  val orderTransitiveOrder = law0(order, "transitive order")
  val orderTotality = law0(order, "totality")
  val orderOrderAndEqualConsistent = law0(order, "order and equal consistent")

  val values: List[SpireLaw] = set.toList

  implicit val spireLawGen: Gen[SpireLaw] = {
    val h :: t = values
    Gen.elements(h, t: _*)
  }

  implicit val spireLawOrder: scalaz.Order[SpireLaw] = {
    import scalaz.std.anyVal._
    scalaz.Order.orderBy(_.ord)
  }
}
