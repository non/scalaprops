package scalaprops

import scalaz._
import scalaz.Isomorphism._
import java.nio.ByteBuffer

abstract class ToFunc[A] { self =>
  def toFunc[B](f: A => B): Func[A, B]

  final def xmap[B](f: A => B, g: B => A): ToFunc[B] =
    new ToFunc[B] {
      def toFunc[C](h: B => C) =
        ToFunc.functionMap[B, A, C](g, f, h)(self)
    }

  final def xmapi[B](iso: A <=> B): ToFunc[B] =
    xmap(iso.to, iso.from)
}

trait Read[A] {
  def read(a: String): A
  final def readF: String => A = read(_)
}

object Read {
  def read[A](f: String => A): Read[A] =
    new Read[A] {
      def read(a: String) = f(a)
    }
}

object ToFunc {
  @inline def apply[A](implicit A: ToFunc[A]): ToFunc[A] = A

  def functionMap[A, B, C](g: A => B, h: B => A, f: A => C)(implicit B: ToFunc[B]): Func[A, C] =
    Func.Map(g, h, B.toFunc(f compose h))

  def functionMapIso[A, B, C](iso: A <=> B, f: A => C)(implicit B: ToFunc[B]): Func[A, C] =
    functionMap(iso.to, iso.from, f)

  private[this] val UnitR = \/-(())
  private[this] val UnitL = -\/(())

  object Iso {
    val maybe: ({type l[a] = Unit \/ a})#l <~> Maybe =
      new IsoFunctorTemplate[({type l[a] = Unit \/ a})#l, Maybe] {
        override def from[A](fa: Maybe[A]) = fa.toRight(())
        override def to[A](ga: Unit \/ A) = ga match {
          case \/-(a) => Maybe.Just(a)
          case -\/(_) => Maybe.empty[A]
        }
      }

    val iList: IList <~> ({type l[a] = Unit \/ LazyTuple2[a, IList[a]]})#l =
      new IsoFunctorTemplate[IList, ({type l[a] = Unit \/ LazyTuple2[a, IList[a]]})#l] {
        def from[A](fa: Unit \/ LazyTuple2[A, IList[A]]) = fa match {
          case \/-(x) => x._1 :: x._2
          case -\/(_) => IList.empty[A]
        }

        def to[A](ga: IList[A]) = ga match {
          case ICons(h, t) => \/-(LazyTuple2(h, t))
          case INil() => UnitL
        }
      }

    val boolean: (Unit \/ Unit) <=> Boolean =
      new Iso[Function1, Unit \/ Unit, Boolean] {
        val from: Boolean => (Unit \/ Unit) = {
          case true => UnitR
          case false => UnitL
        }

        val to: (Unit \/ Unit => Boolean) = _.isRight
      }

    val bigInt: NonEmptyList[Byte] <=> BigInt =
      new Iso[Function1, NonEmptyList[Byte], BigInt] {
        override val from = { n: BigInt =>
          val array = n.toByteArray
          NonEmptyList.nel(array(0), array.drop(1).toList)
        }

        override val to = { bytes: NonEmptyList[Byte] =>
          BigInt(bytes.list.toArray)
        }
      }

    val long: BigInt <=> Long =
      new Iso[Function1, BigInt, Long] {
        override val from: Long => BigInt = BigInt(_)

        override val to: BigInt => Long = _.toLong
      }

    type T4[A] = (A, A, A, A)

    /*
    val int: T4[Byte] <=> Int =
      new Iso[Function1, T4[Byte], Int] {
        override def to: T4[Byte] => Int = { n =>
          ByteBuffer.wrap(Array[Byte](n._1, n._2, n._3, n._4)).getInt
        }

        override def from: Int => T4[Byte] = { n =>
          val buf = ByteBuffer.allocate(4).putInt(n)
          (buf.get(0), buf.get(1), buf.get(2), buf.get(3))
        }
      }

*/

    val nel: ({type l[a] = (a, List[a])})#l <~> NonEmptyList =
      new IsoFunctorTemplate[({type l[a] = (a, List[a])})#l, NonEmptyList] {
        def to[A](fa: (A, List[A])) =
          NonEmptyList.nel(fa._1, fa._2)

        def from[A](ga: NonEmptyList[A]) =
          (ga.head, ga.tail)
      }
  }

  implicit val unit: ToFunc[Unit] =
    new ToFunc[Unit] {
      def toFunc[B](f: Unit => B) =
        Func.Single(f(()))
    }

  implicit val boolean: ToFunc[Boolean] =
    ToFunc[Unit \/ Unit].xmapi(Iso.boolean)

  implicit val byte: ToFunc[Byte] =
    new ToFunc[Byte] {
      import scalaz.std.anyVal._
      def toFunc[B](f: Byte => B) =
        Func.Table(
          (Byte.MinValue to Byte.MaxValue).map{ x =>
            x.asInstanceOf[Byte] -> f(x.asInstanceOf[Byte])
          }.toStream
        )
    }

  implicit val short: ToFunc[Short] =
    new ToFunc[Short] {
      import scalaz.std.anyVal._
      def toFunc[B](f: Short => B) =
        Func.Table(
          (Short.MinValue to Short.MaxValue).map{ x =>
            x.asInstanceOf[Short] -> f(x.asInstanceOf[Short])
          }.toStream
        )
    }


  implicit def lazyTuple2[A1, A2](implicit
    A1: ToFunc[A1],
    A2: ToFunc[A2]
  ): ToFunc[LazyTuple2[A1, A2]] = new ToFunc[LazyTuple2[A1, A2]] {
    def toFunc[B](f: LazyTuple2[A1, A2] => B) =
      Func.Pair {
        A1.toFunc { a1 =>
          A2.toFunc { a2 =>
            f(LazyTuple2(a1, a2))
          }
        }
      }
  }


  implicit def lazyTuple3[A1, A2, A3](implicit
    A1: ToFunc[A1],
    A2: ToFunc[A2],
    A3: ToFunc[A3]
  ): ToFunc[LazyTuple3[A1, A2, A3]] = new ToFunc[LazyTuple3[A1, A2, A3]] {
    def toFunc[B](f: LazyTuple3[A1, A2, A3] => B) =
      functionMap[LazyTuple3[A1, A2, A3], LazyTuple2[A1, LazyTuple2[A2, A3]], B](
        x => LazyTuple2(x._1, LazyTuple2(x._2, x._3)),
        x => LazyTuple3(x._1, x._2._1, x._2._2),
        f
      )
  }


  implicit def disjunction[A1, A2](implicit
    A1: ToFunc[A1],
    A2: ToFunc[A2]
  ): ToFunc[A1 \/ A2] = new ToFunc[A1 \/ A2] {
    def toFunc[B](f: (A1 \/ A2) => B) =
      Func.Sum(
        A1.toFunc(f.compose(\/.left)),
        A2.toFunc(f.compose(\/.right))
      )
  }

  implicit def maybe[A](implicit A: ToFunc[A]): ToFunc[Maybe[A]] =
    ToFunc[Unit \/ A].xmapi(Iso.maybe.unlift)


  implicit def iList[A](implicit A: ToFunc[A]): ToFunc[IList[A]] =
    new ToFunc[IList[A]] {
      def toFunc[B](f: IList[A] => B) =
        functionMapIso(Iso.iList.unlift, f)
    }

  implicit def list[A](implicit A: ToFunc[A]): ToFunc[List[A]] =
    ToFunc[IList[A]].xmapi(IList.listIListIso.flip.unlift[A])

}
