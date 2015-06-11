package scalaprops

import scala.util.Random
import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._

object GenTest extends Scalaprops {

  private implicit def genGen[A](implicit A: Gen[A]): Gen[Gen[A]] = {
    val values = Gen[List[A]].sample(size = 100, seed = Random.nextLong())
    Gen.oneOf(
      Gen.value(A),
      List(
        values.map(
          x => Gen.value(Gen.value(x))
        ),
        List.fill(100)(Random.nextInt(Int.MaxValue - 1) + 1).map(x =>
          Gen.value(A.resize(x))
        ),
        List.fill(100)(Random.nextInt(Int.MaxValue - 1) + 1).map(x =>
          Gen.value(Gen.gen{ (i, r) =>
            val (r0, a) = A.f(i, r)
            (r0.reseed(x), a)
          })
        )
      ).flatten : _*
    )
  }

  implicit def genEqual[A](implicit A: Equal[A]): Equal[Gen[A]] =
    Equal.equal{ (x, y) =>
      Iterator.fill(100)((Random.nextInt(), Random.nextLong())).forall{
        case (size, seed) =>
          val r = Rand.standard(seed)
          Equal[(Rand, A)].equal(x.f(size, r), y.f(size, r))
      }
    }

  val testLaw =
    Properties.list(
      scalazlaws.monad.all[Gen],
      scalazlaws.equal.all[Gen[Int]]
    )

  val `test Gen.elements` = {
    val N = 5

    Property.forAllG(
      Gen.sequenceNList(1000, Gen[Rand]),
      Gen.sequenceNList(5, Gen[Int])
    ){ (rs, xs) =>
      val g = Gen.elements(xs.head, xs.tail: _*)
      val r = rs.map(r => g.f(Int.MaxValue, r)._2)
      Macros.assertEqual(r.toSet, xs.toSet)
      Macros.assertEqual(xs.toSet.size, N)
    }.toCheckWith(Param.rand(Rand.fromSeed())).toProperties("test Gen.element")
  }

  val `test Gen.sequenceNList` = {
    val min = 5
    val max = 30
    val a = - 500
    val b = 20000

    Property.forAllG(
      Gen.choose(min, max).flatMap{ size =>
        Gen.sequenceNList(size, Gen.choose(a, b)).map(size -> _)
      }
    ){ case (size, values) =>
      Macros.assertEq(values.length, size)
      Macros.assert(min <= size && size <= max)
      Macros.assert(values.forall{
        x => a <= x && x <= b
      })
    }
  }

  val `test Gen.frequencey` =
    Property.forAllG(
      Gen.sequenceNList(100, Gen.frequency(
        1 -> Gen.value(true),
        5 -> Gen.value(false)
      ))
    ){ list =>
      val (t, f) = list.partition(identity)
      (t.size < f.size) && t.nonEmpty
    }

  val maybeGen = Property.forAllG(
    Gen[Int], Gen.choose(100, 10000), Gen[Int]
  ){ (size, listSize, seed) =>
    val values = Gen[Maybe[Int]].samples(size = size, listSize = listSize, seed = seed)
    val just = values.count(_.isJust)
    Macros.assertEq(values.size, listSize)
    Macros.assert(just > (listSize / 2))
    Macros.assert(just < listSize)
  }

  val choose = Property.forAll{ (a: Int, b: Int, size: Int, seed: Long) =>
    val x = Gen.choose(a, b).sample(size = size, seed = seed)
    val max = math.max(a, b)
    val min = math.min(a, b)
    (min <= x) && (x <= max)
  }

  val listOfN_1 = Property.forAll{ (size0: Byte, seed: Long) =>
    val size = math.abs(size0.toInt)
    val result = Gen.listOfN(size, Gen[Unit]).map(_.size).sample(seed = seed)
    result <= size
  }

  val listOfN_2 = Property.forAll{ seed: Long =>
    val size = 3
    Macros.assertEqual(Gen.listOfN(size, Gen[Unit]).map(_.size).samples(seed = seed, listSize = 100).distinct.size, size + 1)
  }

  val arrayOfN = Property.forAll{ (size0: Byte, seed: Long) =>
    val size = math.abs(size0.toInt)
    val result = Gen.arrayOfN(size, Gen[Unit]).map(_.size).sample(seed = seed)
    result <= size
  }

  val posLong = Property.forAllG(Gen.positiveLong){_ > 0}
  val posInt = Property.forAllG(Gen.positiveInt){_ > 0}
  val posShort = Property.forAllG(Gen.positiveShort){_ > 0}
  val posByte = Property.forAllG(Gen.positiveByte){_ > 0}

  val negLong = Property.forAllG(Gen.negativeLong){_ < 0}
  val negInt = Property.forAllG(Gen.negativeInt){_ < 0}
  val negShort = Property.forAllG(Gen.negativeShort){_ < 0}
  val negByte = Property.forAllG(Gen.negativeByte){_ < 0}

  val javaEnum = Property.forAll{ seed: Int =>
    val values = Gen[java.util.concurrent.TimeUnit].samples(seed = seed).toSet
    Macros.assertEqual(values, java.util.concurrent.TimeUnit.values().toSet)
  }
}
