package scalaprops

import scalaz._

/**
 * @see [[https://github.com/nick8325/quickcheck/blob/2.8.1/Test/QuickCheck/Function.hs]]
 */
sealed abstract class Func[A, B] extends Product with Serializable {
  def map[C](f: B => C): Func[A, C]
  def toAbstract(d: B): A => B

  final def mkFun(b: B): Fun[A, B] =
    Fun(this, b, toAbstract(b))

  final def string(b: Maybe[B])(implicit A: Show[A], B: Show[B]): String = {
    val list1 = table.map{ x =>
      A.shows(x._1) + "->" + B.shows(x._2)
    }
    val list2 = Foldable[Maybe].toStream(b.map(b => "_->" + B.shows(b)))

    (list1 #::: list2).foldLeft(new java.lang.StringBuilder("{")){
      case (builder, str) => builder.append(str).append(", ")
    }.append("}").toString
  }

  def table: Stream[LazyTuple2[A, B]]

  override final def toString = string(Maybe.empty[B])(Show.showA, Show.showA)
}

object Func {

  private[scalaprops] final case class Pair[A, B, C](a: Func[A, Func[B, C]]) extends Func[LazyTuple2[A, B], C] {
    override def map[D](f: C => D): Func[LazyTuple2[A, B], D] =
      Pair(a.map(_.map(f)))

    override def toAbstract(d: C): LazyTuple2[A, B] => C = {
      t => a.map{
        _.toAbstract(d)(t._2)
      }.toAbstract(d)(t._1)
    }

    override def table =
      a.table.flatMap{ x =>
        x._2.table.map{ y =>
          LazyTuple2(LazyTuple2(x._1, y._1), y._2)
        }
      }
  }

  private[scalaprops] final case class Sum[A, B, C](x: Func[A, C], y: Func[B, C]) extends Func[A \/ B, C] {
    override def map[D](f: C => D): Func[A \/ B, D] =
      Sum(x.map(f), y.map(f))

    override def toAbstract(d: C): (A \/ B) => C = {
      case -\/(a) => x.toAbstract(d)(a)
      case \/-(b) => y.toAbstract(d)(b)
    }

    import scalaz.syntax.either._

    override def table =
      x.table.map{
        case a => LazyTuple2(a._1.left[B], a._2)
      } #::: y.table.map{
        case a => LazyTuple2(a._1.right[A], a._2)
      }
  }

  private[scalaprops] final case class Single[A](a: A) extends Func[Unit, A] {
    override def map[C](f: A => C) =
      Single(f(a))

    override def toAbstract(d: A) =
      _ => a

    override def table =
      Stream(LazyTuple2((), a))
  }

  private[scalaprops] final case class Nil[A, B]() extends Func[A, B] {
    override def map[C](f: B => C): Func[A, C] =
      Nil()

    override def toAbstract(d: B) =
      _ => d

    override def table = Stream.empty
  }

  private[scalaprops] final case class Table[A, B](a: Stream[LazyTuple2[A, B]])(implicit val A: Equal[A]) extends Func[A, B] {
    override def map[C](f: B => C): Func[A, C] =
      Table(a.map{case x => LazyTuple2(x._1, f(x._2))})

    override def toAbstract(d: B): A => B =
      aa => a.collectFirst{
        case x if A.equal(aa, x._1) => x._2
      }.getOrElse(d)

    override def table = a
  }

  private[scalaprops] final case class Map[A, B, C](
    x: A => B, y: B => A, z: Func[B, C]
  ) extends Func[A, C] {

    override def map[D](f: C => D): Func[A, D] =
      Func.Map(x, y, z.map(f))

    override def toAbstract(d: C): A => C =
      x andThen z.toAbstract(d)

    override def table =
      z.table.map{case a => LazyTuple2(y(a._1), a._2)}
  }

  implicit def funcFunctor[C]: Functor[({type l[a] = Func[C, a]})#l] =
    new Functor[({type l[a] = Func[C, a]})#l] {
      def map[A, B](fa: Func[C, A])(f: A => B) =
        fa map f
    }

  implicit def show[A, B](implicit A: Show[A], B: Show[B]): Show[Func[A, B]] =
    Show.shows(_.string(Maybe.empty[B]))

  implicit def gen[A, B](implicit F: ToFunc[A], A: Cogen[A], B: Gen[B]): Gen[Func[A, B]] =
    Gen[A => B].map(F.toFunc)

  implicit def equal[A: Equal, B: Equal]: Equal[Func[A, B]] = {
    import scalaz.std.tuple._
    import scalaz.std.stream._
    Equal.equalBy(_.table.map(x => (x._1, x._2)))
  }
}
