package scalaprops
package spirelaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import spire.algebra.Eq
import spire.syntax.eq._

object equal {

  def reflexive[A](implicit A: Eq[A], G: Gen[A]) =
    forAll { (x: A) =>
      x === x
    }

  def symmetry[A](implicit A: Eq[A], G: Gen[A]) =
    forAll { (x: A, y: A) =>
      (x === y) == (y === x)
    }

  def transitive[A](implicit A: Eq[A], G: Gen[A]) =
    forAll { (x: A, y: A, z: A) =>
      if (x === y && y === z) x === z else true
    }

  def naturality[A](implicit A: Eq[A], G: Gen[A], F: Gen[A => A]) =
    forAll { (x: A, y: A, f: A => A) =>
      if (x === y) f(x) === f(y) else true
    }

  def laws[A](implicit A: Eq[A], G: Gen[A], F: Gen[A => A]): Properties[SpireLaw] =
    properties(SpireLaw.equal)(
      SpireLaw.equalReflexive -> reflexive[A],
      SpireLaw.equalSymmetry -> symmetry[A],
      SpireLaw.equalNaturality -> naturality[A],
      SpireLaw.equalTransitive -> transitive[A]
    )

  def all[A](implicit A: Eq[A], G: Gen[A], F: Gen[A => A]): Properties[SpireLaw] =
    laws[A]
}
