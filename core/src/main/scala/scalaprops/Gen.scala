package scalaprops

import scala.collection.generic.CanBuildFrom
import scala.concurrent.Future
import scalaz._

final case class GenT[F[_], A] private(f: (Int, Rand) => F[(A, Rand)]) {

  def map[B](g: A => B)(implicit F: Functor[F]): GenT[F, B] =
    GenT{ (i, r1) =>
      F.map(f(i, r1)){ case (a, r2) =>
        g(a) -> r2
      }
    }

  def flatMap[B](g: A => GenT[F, B])(implicit F: Bind[F]): GenT[F, B] =
    GenT{ (i, r1) =>
      F.bind(f(i, r1)){ case (a, r2) =>
        g(a).f(i, r2)
      }
    }

  def resize(s: Int): GenT[F, A] =
    mapSize(Function.const(s))

  def mapSize(g: Int => Int): GenT[F, A] =
    GenT[F, A]((s, r) => f(g(s), r))

  import scalaz.Leibniz.===

  /** convenience method for get sample values */
  def samples(size: Int = Param.defaultSize, listSize: Int = 100, seed: Long = Rand.defaultSeed)(implicit e: GenT[F, A] === Gen[A]): List[A] =
    Gen.sequenceNList(listSize, e(this)).f(size, Rand.standard(seed))._1

  def sample(size: Int = Param.defaultSize, seed: Long = Rand.defaultSeed)(implicit F: Functor[F]): F[A] =
    F.map(f(size, Rand.standard(seed)))(_._1)

  def infiniteIterator(size: Int = Param.defaultSize, seed: Long = Rand.defaultSeed)(implicit e: GenT[F, A] === Gen[A]): Iterator[A] =
    Gen.infinite(size, Rand.standard(seed), e(this))

  def infiniteStream(size: Int = Param.defaultSize, seed: Long = Rand.defaultSeed)(implicit e: GenT[F, A] === Gen[A]): Stream[A] =
    Gen.infinite(size, Rand.standard(seed), e(this)).toStream

  def toReaderState(implicit F: Functor[F]): Kleisli[({type l[a] = StateT[F, Rand, a]})#l, Int, A] =
    Gen.toReaderState[F, A](this)

  def toStateReader(implicit F: Functor[F]): StateT[({type l[a] = Kleisli[F, Int, a]})#l, Rand, A] =
    Gen.toStateReader[F, A](this)
}


sealed abstract class GenTInstances1 extends GenInstances {

  implicit final def endomorphicGen[F[_, _], A](implicit F: Gen[F[A, A]]): Gen[Endomorphic[F, A]] =
    F.map(Endomorphic.apply)

  implicit def genTFunctor[F[_]](implicit F0: Functor[F]): Functor[({type l[a] = GenT[F, a]})#l] =
    new GenTFunctor[F] { def F = F0 }

}

sealed abstract class GenTInstances0 extends GenTInstances1 {
  implicit def genTBind[F[_]](implicit F0: Bind[F]): Bind[({type l[a] = GenT[F, a]})#l] =
    new GenTBind[F] { def F = F0 }
}

sealed abstract class GenTInstances extends GenTInstances0 {
  implicit def genTMonad[F[_]](implicit F0: Monad[F]): Monad[({type l[a] = GenT[F, a]})#l] =
    new GenTMonad[F] { def F = F0 }
}

object GenT extends GenTInstances {

  implicit def genTMonadPlus[F[_]](implicit F0: MonadPlus[F]): MonadPlus[({type l[a] = GenT[F, a]})#l] =
    new GenTMonadPlus[F] { def F = F0 }

  private[this] val iListFromList = IList.fromList[Any] _
  private[scalaprops] def IListFromList[A]: List[A] => IList[A] =
    iListFromList.asInstanceOf[List[A] => IList[A]]

  def gen[A](f: (Int, Rand) => (A, Rand)): Gen[A] =
    new Gen(f)

  def genT[F[_], A](f: (Int, Rand) => F[(A, Rand)]): GenT[F, A] =
    new GenT(f)

  def apply[A](implicit A: Gen[A]): Gen[A] = A

  def value[A](a: A): Gen[A] =
    gen((_, r) => (a, r))

  import Isomorphism._

  def isoReaderState[F[_]: Functor]: ({type x[b] = Kleisli[({type l[a] = StateT[F, Rand, a]})#l, Int, b]})#x <~> ({type l[a] = GenT[F, a]})#l =
    new IsoFunctorTemplate[({type x[b] = Kleisli[({type l[a] = StateT[F, Rand, a]})#l, Int, b]})#x, ({type l[a] = GenT[F, a]})#l] {
      def to[A](fa: Kleisli[({type l[a] = StateT[F, Rand, a]})#l, Int, A]) =
        fromReaderState(fa)
      def from[A](ga: GenT[F, A]): Kleisli[({type l[a] = StateT[F, Rand, a]})#l, Int, A] =
        toReaderState(ga)
    }

  def isoStateReader[F[_]: Functor]: ({type x[b] = StateT[({type l[a] = Kleisli[F, Int, a]})#l, Rand, b]})#x <~> ({type l[a] = GenT[F, a]})#l =
    new IsoFunctorTemplate[({type x[b] = StateT[({type l[a] = Kleisli[F, Int, a]})#l, Rand, b]})#x, ({type l[a] = GenT[F, a]})#l] {
      def to[A](fa: StateT[({type l[a] = Kleisli[F, Int, a]})#l, Rand, A]) =
        fromStateReader(fa)
      def from[A](ga: GenT[F, A]): StateT[({type l[a] = Kleisli[F, Int, a]})#l, Rand, A] =
        toStateReader(ga)
    }

  def toReaderState[F[_], A](g: GenT[F, A])(implicit F: Functor[F]): Kleisli[({type l[a] = StateT[F, Rand, a]})#l, Int, A] =
    Kleisli[({type l[a] = StateT[F, Rand, a]})#l, Int, A] { size: Int =>
      StateT { rand: Rand =>
        F.map(g.f(size, rand))(_.swap)
      }
    }

  def fromReaderState[F[_], A](a: Kleisli[({type l[a] = StateT[F, Rand, a]})#l, Int, A])(implicit F: Functor[F]): GenT[F, A] =
    GenT{ (size, rand) =>
      F.map(a.run(size).run(rand))(_.swap)
    }

  def toStateReader[F[_], A](g: GenT[F, A])(implicit F: Functor[F]): StateT[({type l[a] = Kleisli[F, Int, a]})#l, Rand, A] =
    StateT[({type l[a] = Kleisli[F, Int, a]})#l, Rand, A] { rand: Rand =>
      Kleisli { size: Int =>
        F.map(g.f(size, rand))(_.swap)
      }
    }

  def fromStateReader[F[_], A](a: StateT[({type l[a] = Kleisli[F, Int, a]})#l, Rand, A])(implicit F: Functor[F]): GenT[F, A] =
    GenT{ (size, rand) =>
      F.map(a(rand).run(size))(_.swap)
    }

  def oneOf[A](x: Gen[A], xs: Gen[A]*): Gen[A] = {
    val array = (x +: xs).toArray[Any]
    choose(0, xs.length).flatMap(array(_).asInstanceOf[Gen[A]])
  }

  def oneOfLazy[A](x: Need[Gen[A]], xs: Need[Gen[A]]*): Gen[A] = {
    val array = (x +: xs).toArray[Need[Any]]
    choose(0, xs.length).flatMap(array(_).asInstanceOf[Need[Gen[A]]].value)
  }

  private[this] def sequenceN[F[_], A](n: Int, g: Gen[A], f: List[A] => F[A]): Gen[F[A]] =
    sequenceNList(n, g).map(f)

  private[this] def sequenceNIList[A](n: Int, g: Gen[A]): Gen[IList[A]] =
    sequenceN[IList, A](n, g, IListFromList)

  def sequenceNArray[A: reflect.ClassTag](n: Int, g: Gen[A]): Gen[Array[A]] = {
    val array = new Array[A](n)
    @annotation.tailrec
    def loop(size: Int, i: Int, next: Rand): (Array[A], Rand) = {
      if (i < n) {
        val r = g.f(size, next)
        array(i) = r._1
        loop(size, i + 1, r._2)
      } else {
        (array, next)
      }
    }
    gen{ (size, r) =>
      loop(size, 0, r)
    }
  }

  def sequenceNList[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    @annotation.tailrec
    def loop(size: Int, i: Int, next: Rand, acc: List[A]): (List[A], Rand) = {
      if (i < n) {
        val r = g.f(size, next)
        loop(size, i + 1, r._2, r._1 :: acc)
      } else {
        (acc.reverse, next)
      }
    }
    gen{ (size, r) =>
      loop(size, 0, r, List.empty[A])
    }
  }

  def infinite[A](genSize: Int, r: Rand, g: Gen[A]): Iterator[A] =
    new Iterator[A] {
      private[this] var rand = r
      override def next(): A = {
        val x = g.f(genSize, rand)
        rand = x._2
        x._1
      }
      override def hasNext = true
    }

  def parameterised[A](f: (Int, Rand) => Gen[A]): Gen[A] =
    gen((i, r) => f(i, r).f(i, r))

  def sized[A](f: Int => Gen[A]): Gen[A] =
    parameterised((i, _) => f(i))

  def chooseLong(from: Long, to: Long): Gen[Long] =
    gen{ (_, r) => r.chooseLong(from, to) }

  def choose(from: Int, to: Int): Gen[Int] =
    gen{ (_, r) => r.choose(from, to) }

  def chooseR(from: Int, to: Int, r: Rand): Gen[Int] =
    gen{ (_, _) =>
      r.choose(from, to)
    }

  @annotation.tailrec
  private[this] def pick0[B](n: Int, gs: IList[(Int, Gen[B])]): Gen[B] = gs match {
    case INil() => sys.error(s"bug? $n $gs")
    case ICons(h, t) =>
      val k = h._1
      if (n <= k) h._2
      else pick0(n - k, t)
  }

  def frequency[A](gs: NonEmptyList[(Int, Gen[A])]): Gen[A] = {
    import std.anyVal._
    val F = Foldable[NonEmptyList]
    choose(1, F.foldMap(gs)(_._1)).flatMap{ i =>
      pick0(i, F.toIList(gs))
    }
  }

  def frequency[A](g: (Int, Gen[A]), gs: (Int, Gen[A]) *): Gen[A] =
    frequency(NonEmptyList.nels(g, gs: _*))

  def elemFrequency[A](as: NonEmptyList[(Int, A)]): Gen[A] =
    frequency(as.map{case (i, a) => i -> Gen.value(a)})

  def elements[A](a: A, as: A*): Gen[A] = {
    val xs = (a +: as).toArray[Any]
    choose(0, as.length).map{xs(_).asInstanceOf[A]}
  }

  private[this] def listOf_[F[_], A](g: Gen[A], x: Int, f: List[A] => F[A]): Gen[F[A]] =
    parameterised{ (size, r) =>
      chooseR(x, size, r).flatMap{ n =>
        sequenceN(n, g, f)
      }
    }

  private[this] def arrayOf[A: reflect.ClassTag](g: Gen[A], x: Int): Gen[Array[A]] =
    parameterised{ (size, r) =>
      chooseR(x, size, r).flatMap{ n =>
        sequenceNArray(n, g)
      }
    }

  private[this] def listOfCBF[F[_], A](g: Gen[A], x: Int)(implicit F: CanBuildFrom[Nothing, A, F[A]]): Gen[F[A]] =
    listOf_[F, A](g, x, _.to[F])

  private[this] def listOfCBF0[F[_], A](g: Gen[A])(implicit F: CanBuildFrom[Nothing, A, F[A]]): Gen[F[A]] =
    listOfCBF[F, A](g, 0)

  def listOf[A](g: Gen[A], x: Int = 0): Gen[IList[A]] =
    listOf_[IList, A](g, x, IListFromList)

  implicit def ilist[A](implicit A: Gen[A]): Gen[IList[A]] =
    listOf(A)

  implicit def vector[A](implicit A: Gen[A]): Gen[Vector[A]] =
    listOfCBF0[Vector, A](A)

  implicit def mapGen[A: Gen, B: Gen]: Gen[Map[A, B]] =
    list[(A, B)].map(_.toMap)

  implicit def setGen[A: Gen]: Gen[Set[A]] =
    list[A].map(_.toSet)

  implicit def streamGen[A](implicit A: Gen[A]): Gen[Stream[A]] =
    listOfCBF0[Stream, A](A)

  implicit def list[A](implicit A: Gen[A]): Gen[List[A]] =
    listOfCBF0[List, A](A)

  implicit def arrayGen[A: reflect.ClassTag: Gen]: Gen[Array[A]] =
    Gen[List[A]].map(_.toArray)

  def listOf1[A](g: Gen[A]): Gen[IList[A]] =
    listOf(g, 1)

  private[this] def pick[A](n: Int, as: IList[A]): Gen[IList[A]] = {
    val len = as.length
    if(n < 0 || n > len) {
      sys.error(s"bug $n $as")
    } else {
      sequenceNIList(n, choose(0, len - 1)).map{ is =>
        @annotation.tailrec
        def loop(iis: IList[Int], aas: IList[(A, Int)], acc: IList[A]): IList[A] =
          (iis, aas) match {
            case (ICons(h1, t1), ICons(h2, t2)) =>
              if(h1 == h2._2) loop(t1, t2, acc)
              else loop(iis, t2, h2._1 :: acc)
            case _ =>
              acc.reverse
          }

        import std.anyVal._
        loop(is.sorted, as.zipWithIndex, IList.empty[A])
      }
    }
  }

  def someOf[A](as: IList[A]): Gen[IList[A]] =
    choose(0, as.length).flatMap(i => pick(i, as))

  def promote[A, B](f: A => Gen[B]): Gen[A => B] =
    gen((i, r) => (a => f(a).f(i, r)._1, r))

  val genInstance: Monad[Gen] =
    genTMonad[scalaz.Id.Id]

  implicit val genBoolean: Gen[Boolean] =
    elements(true, false)

  val genIntSized: Gen[Int] =
    parameterised((i, r) => chooseR(-i, i, r))

  val genIntAll: Gen[Int] =
    gen((_, r) => r.nextInt)

  val genLongAll: Gen[Long] =
    gen((_, r) => r.nextLong)

  val genByteAll: Gen[Byte] =
    genIntAll.map(_.toByte)

  val genCharAll: Gen[Char] =
    genIntAll.map(_.toChar)

  val genShortAll: Gen[Short] =
    genIntAll.map(_.toShort)

  val genByteSized: Gen[Byte] =
    genIntSized.map(_.toByte)

  val genCharSized: Gen[Char] =
    genIntSized.map(_.toChar)

  val genShortSized: Gen[Short] =
    genIntSized.map(_.toShort)

  val asciiChar: Gen[Char] =
    choose('!', '~').map(_.toChar)

  implicit val genAsciiChar: Gen[Char @@ GenTags.Ascii] =
    GenTags.Ascii.subst(asciiChar)

  val numChar: Gen[Char] =
    choose('0', '9').map(_.toChar)

  implicit val genNumChar: Gen[Char @@ GenTags.Num] =
    GenTags.Num.subst(numChar)

  val alphaUpperChar: Gen[Char] =
    choose('A', 'Z').map(_.toChar)

  implicit val genAlphaUpperChar: Gen[Char @@ GenTags.AlphaUpper] =
    GenTags.AlphaUpper.subst(alphaUpperChar)

  val alphaLowerChar: Gen[Char] =
    choose('a', 'z').map(_.toChar)

  implicit val genAlphaLowerChar: Gen[Char @@ GenTags.AlphaLower] =
    GenTags.AlphaLower.subst(alphaLowerChar)

  val alphaChar: Gen[Char] =
    Gen.oneOf(alphaLowerChar, alphaUpperChar)

  implicit val genAlphaChar: Gen[Char @@ GenTags.Alpha] =
    GenTags.Alpha.subst(alphaChar)

  val alphaNumChar: Gen[Char] =
    Gen.oneOf(alphaLowerChar, alphaUpperChar, numChar)

  implicit val genAlphaNumChar: Gen[Char @@ GenTags.AlphaNum] =
    GenTags.AlphaNum.subst(alphaNumChar)

  private[this] def genString(g: Gen[Char]): Gen[String] =
    arrayOf(g, 0).map(String.valueOf)

  val numString: Gen[String] =
    genString(numChar)

  implicit val genNumString: Gen[String @@ GenTags.Num] =
    GenTags.Num.subst(numString)

  val alphaUpperString: Gen[String] =
    genString(alphaUpperChar)

  implicit val genAlphaUpperString: Gen[String @@ GenTags.AlphaUpper] =
    GenTags.AlphaUpper.subst(alphaUpperString)

  val alphaLowerString: Gen[String] =
    genString(alphaLowerChar)

  implicit val genAlphaLowerString: Gen[String @@ GenTags.AlphaLower] =
    GenTags.AlphaLower.subst(alphaLowerString)

  val alphaString: Gen[String] =
    genString(alphaChar)

  implicit val genAlphaString: Gen[String @@ GenTags.Alpha] =
    GenTags.Alpha.subst(alphaString)

  val alphaNumString: Gen[String] =
    genString(alphaNumChar)

  implicit val genAlphaNumString: Gen[String @@ GenTags.AlphaNum] =
    GenTags.AlphaNum.subst(alphaNumString)

  val asciiString: Gen[String] =
    genString(asciiChar)

  implicit val genAsciiString: Gen[String @@ GenTags.Ascii] =
    GenTags.Ascii.subst(asciiString)

  implicit val genUnit: Gen[Unit] =
    value(())

  implicit def maybe[A](implicit A: Gen[A]): Gen[Maybe[A]] =
    Gen.frequency(
      1 -> Gen.value(Maybe.empty[A]),
      20 -> A.map(Maybe.just)
    )

  implicit def option[A](implicit A: Gen[A]): Gen[Option[A]] =
    maybe[A].map(_.toOption)

  implicit val genIntBoundaries: Gen[Int] =
    frequency(
      1 -> value(0),
      1 -> value(1),
      1 -> value(-1),
      1 -> value(Int.MaxValue),
      1 -> value(Int.MaxValue - 1),
      1 -> value(Int.MinValue),
      1 -> value(Int.MinValue + 1),
      93 -> choose(Int.MinValue, Int.MaxValue)
    )

  implicit val genLongBoundaries: Gen[Long] =
    frequency(
      1 -> value(0L),
      1 -> value(1L),
      1 -> value(-1L),
      1 -> value(Long.MaxValue),
      1 -> value(Long.MaxValue - 1),
      1 -> value(Long.MinValue),
      1 -> value(Long.MinValue + 1),
      93 -> genLongAll
    )

  implicit val genByteBoundaries: Gen[Byte] =
    frequency(
      1 -> value(0: Byte),
      1 -> value(1: Byte),
      1 -> value(-1: Byte),
      1 -> value(Byte.MaxValue),
      1 -> value(126: Byte),
      1 -> value(Byte.MinValue),
      1 -> value(-127: Byte),
      93 -> genByteAll
    )

  implicit val genShortBoundaries: Gen[Short] =
    frequency(
      1 -> value(0: Short),
      1 -> value(1: Short),
      1 -> value(-1: Short),
      1 -> value(Short.MaxValue),
      1 -> value((Short.MaxValue - 1).asInstanceOf[Short]),
      1 -> value(Short.MinValue),
      1 -> value((Short.MinValue + 1).asInstanceOf[Short]),
      93 -> genShortAll
    )

  implicit val javaIntegerGen: Gen[java.lang.Integer] =
    Gen[Int].map(Integer.valueOf)

  implicit val javaLongGen: Gen[java.lang.Long] =
    Gen[Long].map(java.lang.Long.valueOf)

  implicit val javaByteGen: Gen[java.lang.Byte] =
    Gen[Byte].map(java.lang.Byte.valueOf)

  implicit val javaShortGen: Gen[java.lang.Short] =
    Gen[Short].map(java.lang.Short.valueOf)

  implicit val javaBooleanGen: Gen[java.lang.Boolean] =
    Gen.elements(java.lang.Boolean.TRUE, java.lang.Boolean.FALSE)

  implicit def lazyTuple2[A1, A2](implicit A1: Gen[A1], A2: Gen[A2]): Gen[LazyTuple2[A1, A2]] =
    Apply[Gen].apply2(A1, A2)(LazyTuple2(_, _))

  implicit def lazyTuple3[A1, A2, A3](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3]): Gen[LazyTuple3[A1, A2, A3]] =
    Apply[Gen].apply3(A1, A2, A3)(LazyTuple3(_, _, _))

  implicit def lazyTuple4[A1, A2, A3, A4](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4]): Gen[LazyTuple4[A1, A2, A3, A4]] =
    Apply[Gen].apply4(A1, A2, A3, A4)(LazyTuple4(_, _, _, _))

  implicit def lazyOptionGen[A: Gen]: Gen[LazyOption[A]] =
    Gen[Maybe[A]].map{
      case Maybe.Just(a) => LazyOption.lazySome(a)
      case Maybe.Empty() => LazyOption.lazyNone[A]
    }

  implicit def lazyEitherGen[A1, A2](implicit A1: Gen[A1], A2: Gen[A2]): Gen[LazyEither[A1, A2]] =
    oneOf(
      A1.map(LazyEither.lazyLeft[A2].apply(_)),
      A2.map(LazyEither.lazyRight[A1].apply(_))
    )

  implicit def tuple1[A1](implicit A1: Gen[A1]): Gen[Tuple1[A1]] =
    A1.map(Tuple1.apply)

  implicit def imapGen[A: Order: Gen, B: Gen]: Gen[A ==>> B] =
    Gen[List[(A, B)]].map(==>>.fromList(_))

  implicit def isetGen[A: Order: Gen]: Gen[ISet[A]] =
    Gen[List[A]].map(ISet.fromList(_))

  implicit def kleisli[F[_], A, B](implicit F: Gen[A => F[B]]): Gen[Kleisli[F, A, B]] =
    F.map(Kleisli(_))

  implicit def cokleisli[F[_], A, B](implicit F: Gen[F[A] => B]): Gen[Cokleisli[F, A, B]] =
    F.map(Cokleisli(_))

  implicit def nullResult[A, B](implicit F: Gen[A => Option[B]]): Gen[NullResult[A, B]] =
    F.map(NullResult(_))

  implicit def nullArgument[A, B](implicit F: Gen[Option[A] => B]): Gen[NullArgument[A, B]] =
    F.map(NullArgument(_))

  implicit def nonEmptyList[A](implicit A: Gen[A]): Gen[NonEmptyList[A]] =
    Apply[Gen].apply2(A, Gen[List[A]])(NonEmptyList.nel)

  implicit def oneAnd[F[_], A](implicit F: Gen[F[A]], A: Gen[A]): Gen[OneAnd[F, A]] =
    Apply[Gen].apply2(A, F)(OneAnd(_, _))

  implicit def oneOr[F[_], A](implicit F: Gen[F[A]], A: Gen[A]): Gen[OneOr[F, A]] =
    Gen[F[A] \/ A].map(OneOr.apply)

  implicit def dequeueGen[A](implicit A: Gen[A]): Gen[Dequeue[A]] =
    Gen.oneOf(
      Gen.value(Dequeue.empty[A]),
      A.map(Dequeue.apply(_)),
      Apply[Gen].apply2(Gen[NonEmptyIList[A]], Gen[NonEmptyIList[A]])(
        (x, y) => Dequeue.fromFoldable(x) ++ Dequeue.fromFoldable(y)
      )
    )

  implicit def disjunction[A, B](implicit A: Gen[A], B: Gen[B]): Gen[A \/ B] =
    oneOf(A.map(\/.left), B.map(\/.right))

  implicit def eitherGen[A, B](implicit A: Gen[A], B: Gen[B]): Gen[A Either B] =
    oneOf(B.map(Right(_)), A.map(Left(_)))

  implicit def lazyEitherTGen[F[_], A, B](implicit F: Gen[F[LazyEither[A, B]]]): Gen[LazyEitherT[F, A, B]] =
    F.map(LazyEitherT(_))

  implicit def eitherTGen[F[_], A, B](implicit F: Gen[F[A \/ B]]): Gen[EitherT[F, A, B]] =
    F.map(EitherT(_))

  implicit def maybeTGen[F[_], A](implicit F: Gen[F[Maybe[A]]]): Gen[MaybeT[F, A]] =
    F.map(MaybeT.apply(_))

  implicit def optionTGen[F[_], A](implicit F: Gen[F[Option[A]]]): Gen[OptionT[F, A]] =
    F.map(OptionT.apply(_))

  implicit def lazyOptionTGen[F[_], A](implicit F: Gen[F[LazyOption[A]]]): Gen[LazyOptionT[F, A]] =
    F.map(LazyOptionT(_))

  implicit def idTGen[F[_], A](implicit F: Gen[F[A]]): Gen[IdT[F, A]] =
    F.map(IdT(_))

  implicit def coproductGen[F[_], G[_], A](implicit F: Gen[F[A] \/ G[A]]): Gen[Coproduct[F, G, A]] =
    F.map(Coproduct(_))

  implicit def constGen[A, B](implicit A: Gen[A]): Gen[Const[A, B]] =
    A.map(Const(_))

  implicit def writerTGen[F[_], A, B](implicit F: Gen[F[(A, B)]]): Gen[WriterT[F, A, B]] =
    F.map(WriterT(_))

  implicit def unwriterTGen[F[_], A, B](implicit F: Gen[F[(A, B)]]): Gen[UnwriterT[F, A, B]] =
    F.map(UnwriterT(_))

  implicit def indexedStateTGen[F[_], S1, S2, A](implicit F: Gen[S1 => F[(S2, A)]]): Gen[IndexedStateT[F, S1, S2, A]] =
    F.map(IndexedStateT(_))

  implicit def indexedContsTGen[W[_], M[_], R, O, A](implicit F: Gen[W[A => M[O]] => M[R]]): Gen[IndexedContsT[W, M, R, O, A]] =
    F.map(IndexedContsT(_))

  implicit def indexedReaderWriterStateTGen[F[_], R, W, S1, S2, A](implicit F: Gen[(R, S1) => F[(W, A, S2)]]): Gen[IndexedReaderWriterStateT[F, R, W, S1, S2, A]] =
    F.map(IndexedReaderWriterStateT.apply)

  implicit def streamTGen[F[_]: Applicative, A](implicit F: Gen[F[Stream[A]]]): Gen[StreamT[F, A]] =
    F.map(StreamT.fromStream(_))

  implicit def theseGen[A, B](implicit A: Gen[A], B: Gen[B]): Gen[A \&/ B] =
    Gen.oneOf(
      A.map(\&/.This.apply),
      B.map(\&/.That.apply),
      Apply[Gen].apply2(A, B)(\&/.Both.apply)
    )

  implicit def listTGen[F[_], A](implicit F: Gen[F[List[A]]]): Gen[ListT[F, A]] =
    F.map(ListT.apply(_))

  implicit def dlistGen[A: Gen]: Gen[DList[A]] =
    Gen[List[A]].map(DList.fromList(_))

  implicit def heapGen[A: Gen: Order]: Gen[Heap[A]] =
    Gen[IList[A]].map(Heap.fromCodata(_))

  implicit def ephemeralStreamGen[A: Gen]: Gen[EphemeralStream[A]] =
    Gen[Stream[A]].map(EphemeralStream.fromStream(_))

  implicit def monoidCoproduct[A: Gen, B: Gen]: Gen[A :+: B] =
    Gen[Vector[A \/ B]].map(new :+:(_))


  implicit def indexedStoreTGen[F[_], I: Gen, A, B](implicit F: Gen[F[A => B]]): Gen[IndexedStoreT[F, I, A, B]] =
    Gen[(F[A => B], I)].map(IndexedStoreT(_))

  implicit val orderingGen: Gen[Ordering] =
    elements(
      scalaz.Ordering.GT,
      scalaz.Ordering.EQ,
      scalaz.Ordering.LT
    )

  implicit def validationGen[A: Gen, B: Gen]: Gen[Validation[A, B]] =
    Gen[A \/ B].map(_.validation)

  implicit def zipperGen[A: Gen]: Gen[Zipper[A]] =
    Gen[(Stream[A], A, Stream[A])].map{case (a, b, c) => Zipper(a, b, c)}

  implicit def coyonedaGen[F[_], A](implicit F: Gen[F[A]]): Gen[Coyoneda[F, A]] =
    F.map(Coyoneda.lift)

  implicit def codensityGen[F[_]: Monad, A](implicit F: Gen[F[A]], A: Gen[A]): Gen[Codensity[F, A]] =
    Gen.oneOf(
      F.map(Codensity.rep[F, A]),
      A.map(Codensity.pureCodensity(_))
    )

  implicit def indSeqGen[A: Gen]: Gen[IndSeq[A]] =
    Gen[List[A]].map(IndSeq.fromSeq)

  implicit def dievGen[A: Gen: Enum]: Gen[Diev[A]] =
    Gen[List[A]].map(Diev.fromValuesSeq(_))

  implicit def either3Gen[A1, A2, A3](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3]): Gen[Either3[A1, A2, A3]] =
    Gen.oneOf(
      A1.map(Either3.left3),
      A2.map(Either3.middle3),
      A3.map(Either3.right3)
    )

  implicit def immutableArrayGen[A: Gen: reflect.ClassTag]: Gen[ImmutableArray[A]] =
    Gen[Array[A]].map(ImmutableArray.fromArray)

  implicit def nameGen[A](implicit A: Gen[A]): Gen[Name[A]] =
    A.map(Name.apply(_))

  implicit def valueGen[A](implicit A: Gen[A]): Gen[Value[A]] =
    A.map(Value.apply(_))

  implicit def needGen[A](implicit A: Gen[A]): Gen[Need[A]] =
    A.map(Need.apply(_))

  implicit def futureGen[A](implicit A: Gen[A]): Gen[Future[A]] =
    A.map(Future.successful(_))

  implicit def endoGen[A: Gen: Cogen]: Gen[Endo[A]] =
    Gen[A => A].map(Endo(_))

  implicit def kleisliLikeEndoGen[G[_[_], _, _], F[_], A](implicit F: Gen[G[F, A, A]]): Gen[Endomorphic[({type l[a, b] = G[F, a, b]})#l, A]] =
    endomorphicGen[({type l[a, b] = G[F, a, b]})#l, A]

  implicit def contravariantCoyonedaGen[F[_], A](implicit F: Gen[F[A]]): Gen[ContravariantCoyoneda[F, A]] =
    F.map(ContravariantCoyoneda.lift)

  implicit def fingerGen[V, A](implicit A: Gen[A], R: Reducer[A, V]): Gen[Finger[V, A]] =
    Gen.oneOf(
      A.map(FingerTree.one[V, A]),
      Apply[Gen].apply2(A, A)(FingerTree.two[V, A]),
      Apply[Gen].apply3(A, A, A)(FingerTree.three[V, A]),
      Apply[Gen].apply4(A, A, A, A)(FingerTree.four[V, A])
    )

  implicit def treeGen[A](implicit A: Gen[A]): Gen[Tree[A]] = {
    def loop(size: Int): Gen[Tree[A]] = size match {
      case n if n <= 1 =>
        A.map(a => Tree.leaf(a))
      case 2 =>
        Gen[(A, A)].map{case (a1, a2) =>
          Tree.node(a1, Stream(Tree.leaf(a2)))
        }
      case 3 =>
        Gen[(A, A, A)].flatMap{case (a1, a2, a3) =>
          Gen.elements(
            Tree.node(a1, Stream(Tree.leaf(a2), Tree.leaf(a3))),
            Tree.node(a1, Stream(Tree.node(a2, Stream(Tree.leaf(a3)))))
          )
        }
      case _ =>
        val max = size - 1
        import scalaz.std.stream._
        Applicative[Gen].sequence(
          Stream.fill(max)(Gen.choose(1, max - 1))
        ).flatMap{s =>
          val ns = Traverse[Stream].traverseS(s){ n =>
            for{
              sum <- State.get[Int]
              r <- if(sum >= max){
                State.state[Int, Option[Int]](None)
              }else if((sum + n) > max){
                State((s: Int) => (s + n) -> Option(max - sum))
              }else{
                State((s: Int) => (s + n) -> Option(n))
              }
            } yield r
          }.eval(0).flatten

          Applicative[Gen].sequence(ns.map(loop)).flatMap(as =>
            A.map(a => Tree.node(a, as))
          )
        }
    }
    Gen.sized(n =>
      Gen.choose(0, n).flatMap(loop)
    )
  }

  implicit def shrinkGen[A: Cogen: Gen]: Gen[Shrink[A]] =
    Gen[A => Stream[A]].map(new Shrink(_))

  implicit val orEmptyGen: Gen[Or.Empty] =
    Gen.oneOfLazy(Need(???))

  implicit def orGen[A, B <: Or](implicit A: Gen[A], B: Gen[B]): Gen[A :-: B] = {
    if(B eq orEmptyGen){
      A.map[A :-: B](Or.L(_))
    }else {
      Gen.frequency(
        1 -> A.map[A :-: B](Or.L(_)),
        3 -> B.map[A :-: B](Or.R(_))
      )
    }
  }

  implicit def partialFunctionGen[A: Cogen, B: Gen]: Gen[PartialFunction[A, B]] =
    Gen[A => Option[B]].map(Function.unlift)

  implicit def genTGen[F[_]: Functor, A](implicit F: Gen[Kleisli[({type l[a] = StateT[F, Rand, a]})#l, Int, A]]): Gen[GenT[F, A]] =
    F.map(fromReaderState(_))
}


private trait GenTFunctor[F[_]] extends Functor[({type l[a] = GenT[F, a]})#l] {
  protected[this] implicit def F: Functor[F]

  override final def map[A, B](fa: GenT[F, A])(f: A => B) =
    fa map f
}

private trait GenTBind[F[_]] extends GenTFunctor[F] with Bind[({type l[a] = GenT[F, a]})#l]{
  protected[this] implicit def F: Bind[F]

  override final def bind[A, B](fa: GenT[F, A])(f: A => GenT[F, B]) =
    fa flatMap f
}

private abstract class GenTMonad[F[_]] extends Monad[({type l[a] = GenT[F, a]})#l] with GenTBind[F]{
  protected[this] implicit def F: Monad[F]

  override final def point[A](a: => A) =
    GenT.genT((_, r) => F.point((a, r)))
}

private abstract class GenTMonadPlus[F[_]] extends IsomorphismMonadPlus[({type l[a] = GenT[F, a]})#l, ({type x[b] = Kleisli[({type l[a] = StateT[F, Rand, a]})#l, Int, b]})#x]{
  protected[this] implicit def F: MonadPlus[F]
  override final def G = Kleisli.kleisliMonadPlus[({type l[a] = StateT[F, Rand, a]})#l, Int]
  override def iso = GenT.isoReaderState[F](F).flip
}
