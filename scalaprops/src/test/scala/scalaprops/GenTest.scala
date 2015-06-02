package scalaprops

import scala.util.Random
import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.std.tuple._

object GenTest extends Scalaprops {

  private implicit def genGen[A](implicit A: Gen[A]): Gen[Gen[A]] = {
    val values = Gen[List[A]].f(100, Rand.standard(Random.nextLong()))._1
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
            val (a, r0) = A.f(i, r)
            (a, r0.reseed(x))
          })
        )
      ).flatten : _*
    )
  }

  implicit def genTEqual[F[_], A](implicit A: Equal[F[(A, Rand)]]): Equal[GenT[F, A]] =
    Equal.equal{ (x, y) =>
      Iterator.fill(100)((Random.nextInt(), Random.nextLong())).forall{
        case (size, seed) =>
          val r = Rand.standard(seed)
          A.equal(x.f(size, r), y.f(size, r))
      }
    }

  val testLaw =
    Properties.either(
      "Gen",
      scalazlaws.monad.all[Gen],
      scalazlaws.equal.all[Gen[Int]]
    )

  val `GenT[Maybe, _]` = {
    implicit def f[A: Gen] = GenT.genTGen[Maybe, A](
      implicitly,
      Gen.kleisli[({type l[a] = StateT[Maybe, Rand, a]})#l, Int, A]
    )

    Properties.list(
      scalazlaws.monadPlusStrong.all[({type l[a] = GenT[Maybe, a]})#l],
      scalazlaws.equal.all[GenT[Maybe, Int]]
    )
  }

  val `GenT[IList, _]` = {
    implicit def f[A: Gen] = GenT.genTGen[IList, A](
      implicitly,
      Gen.kleisli[({type l[a] = StateT[IList, Rand, a]})#l, Int, A]
    )

    Properties.list(
      scalazlaws.monadPlusStrong.all[({type l[a] = GenT[IList, a]})#l],
      scalazlaws.equal.all[GenT[IList, Int]]
    ).andThenParam(Param.maxSize(5))
  }


  val `test Gen.elements` = {
    val N = 5

    Property.forAllG(
      Gen.sequenceNList(1000, Gen[Rand]),
      Gen.sequenceNList(5, Gen[Int])
    ){ (rs, xs) =>
      val g = Gen.elements(xs.head, xs.tail: _*)
      val r = rs.map(r => g.f(Int.MaxValue, r)._1)
      (r.toSet == xs.toSet) && (xs.toSet.size == N)
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
      (values.length == size) && (min <= size && size <= max) && values.forall{
        x => a <= x && x <= b
      }
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
    (values.size == listSize) && (just > (listSize / 2)) && (just < listSize)
  }

  val choose = Property.forAll{ (a: Int, b: Int, size: Int, seed: Int) =>
    val x = Gen.choose(a, b).f(size, Rand.fromSeed(seed))._1
    val max = math.max(a, b)
    val min = math.min(a, b)
    (min <= x) && (x <= max)
  }

  object instances {
    def functor[F[_]: Functor, A] = Functor[({type l[a] = GenT[F, a]})#l]
    def functor[F[_]: Bind, A] = Functor[({type l[a] = GenT[F, a]})#l]
    def functor[F[_]: Monad, A] = Functor[({type l[a] = GenT[F, a]})#l]
    def functor[F[_]: MonadPlus, A] = Functor[({type l[a] = GenT[F, a]})#l]

    def bind[F[_]: Bind, A] = Bind[({type l[a] = GenT[F, a]})#l]
    def bind[F[_]: Monad, A] = Bind[({type l[a] = GenT[F, a]})#l]
    def bind[F[_]: MonadPlus, A] = Bind[({type l[a] = GenT[F, a]})#l]

    def monad[F[_]: Monad, A] = Monad[({type l[a] = GenT[F, a]})#l]
    def monad[F[_]: MonadPlus, A] = Monad[({type l[a] = GenT[F, a]})#l]

    def monadPlus[F[_]: MonadPlus, A] = MonadPlus[({type l[a] = GenT[F, a]})#l]
  }
}
