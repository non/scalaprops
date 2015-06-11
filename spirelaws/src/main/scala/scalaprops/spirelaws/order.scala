package scalaprops
package spirelaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import scala.math.{Ordering => SOrdering}
import spire.algebra.Order
import spire.syntax.order._

object order {
  def antisymmetric[A](implicit A: Order[A], G: Gen[A]): Property =
    forAll { (x: A, y: A) =>
      (x <= y && x >= y) == (x === y)
    }

  def transitiveOrder[A](implicit A: Order[A], G: Gen[A]): Property =
    forAll { (x: A, y: A, z: A) =>
      if (x <= y && y <= z) x <= z
      else if (x >= y && y >= z) x >= z
      else true
    }

  def totality[A](implicit A: Order[A], G: Gen[A]): Property =
    forAll { (x: A, y: A) =>
      (x >= y) || (x <= y)
    }

  def orderAndEqualConsistent[A](implicit A: Order[A], G: Gen[A]): Property =
    forAll { (x: A, y: A) =>
      ((x compare y) == 0) == (x === y)
    }

  def laws[A: Order : Gen]: Properties[SpireLaw] =
    properties(SpireLaw.order)(
      SpireLaw.orderAntisymmetric -> antisymmetric[A],
      SpireLaw.orderTransitiveOrder -> transitiveOrder[A],
      SpireLaw.orderTotality -> totality[A],
      SpireLaw.orderOrderAndEqualConsistent -> orderAndEqualConsistent[A]
    )

  def all[A](implicit A: Order[A], G: Gen[A], F: Gen[A => A]): Properties[SpireLaw] =
    Properties.fromProps(SpireLaw.orderAll, order.laws[A], equal.laws[A])
}
