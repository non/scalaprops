package scalaprops

import scalaz._
import scalaz.std.anyVal._

object EitherTTest extends Scalaprops {

  val testEitherTMaybe =
    Properties.list(
      scalazlaws.monadPlus.all[({type l[a] = EitherT[Maybe, Int, a]})#l],
      scalazlaws.traverse.all[({type l[a] = EitherT[Maybe, Int, a]})#l]
    )

  val testEitherTMaybe2 =
    scalazlaws.bitraverse.all[({type l[a, b] = EitherT[Maybe, a, b]})#l]

  val testEitherTIList =
    Properties.list(
      scalazlaws.monadPlus.all[({type l[a] = EitherT[IList, Int, a]})#l],
      scalazlaws.traverse.all[({type l[a] = EitherT[IList, Int, a]})#l]
    )

  val testEitherTNel =
    Properties.list(
      scalazlaws.monadPlus.all[({type l[a] = EitherT[NonEmptyList, Int, a]})#l],
      scalazlaws.traverse.all[({type l[a] = EitherT[NonEmptyList, Int, a]})#l]
    )

}
