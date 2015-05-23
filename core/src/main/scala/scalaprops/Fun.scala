package scalaprops

import scalaz._

final case class Fun[A, B](f: Func[A, B], b: B, fun: A => B)

object Fun {

  implicit def show[A, B](implicit A: Show[A], B: Show[B]): Show[Fun[A, B]] =
    Show.shows{ f =>
      f.f.string(Maybe.Just(f.b))
    }

  implicit def gen[A, B](implicit F: ToFunc[A], A: Cogen[A], B: Gen[B]): Gen[Fun[A, B]] =
    Apply[Gen].apply2(
      Gen[Func[A, B]], B
    )(_.mkFun(_))

}
