package scalaprops

import scalaz._
import scalaz.std.anyVal._

object IndexedContsTTest extends Scalaprops {

  private[this] val F = new FunctionEqual(5)
  import F._

  private[this] implicit def indexedContsTEqual[W[_], M[_], R, O, A](implicit F: Equal[W[A => M[O]] => M[R]]): Equal[IndexedContsT[W, M, R, O, A]] =
    F.contramap(_.run)

  val testContsTMaybe = {
    type F[A] = ContsT[NonEmptyList, Maybe, Int, A]
    scalazlaws.monad.all[F].andThenParam(Param.maxSize(4))
  }

  val testContsTIList = {
    type F[A] = ContsT[NonEmptyList, IList, Int, A]
    scalazlaws.monad.all[F]
  }

  val testCont = {
    type F[A] = Conts[NonEmptyList, Int, A]
    scalazlaws.monad.all[F]
  }

  val testBifunctorMaybeMaybe =
    scalazlaws.bifunctor.all[({type F[A, B] = IndexedContsT[Maybe, IList, A, Int, B]})#F]

  val testBifunctorMaybeIList =
    scalazlaws.bifunctor.all[({type F[A, B] = IndexedContsT[Maybe, IList, A, Int, B]})#F]

  val testBifunctorIListMaybe =
    scalazlaws.bifunctor.all[({type F[A, B] = IndexedContsT[IList, Maybe, A, Int, B]})#F]

  val testContravariantMaybeMaybe =
    scalazlaws.contravariant.all[({type F[A] = IndexedContsT[Maybe, IList, Int, A, Int]})#F]

  val testContravariantMaybeIList =
    scalazlaws.contravariant.all[({type F[A] = IndexedContsT[Maybe, IList, Int, A, Int]})#F]

  val testContravariantIListMaybe =
    scalazlaws.contravariant.all[({type F[A] = IndexedContsT[IList, Maybe, Int, A, Int]})#F]
}
