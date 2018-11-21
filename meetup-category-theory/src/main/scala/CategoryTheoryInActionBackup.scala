import scala.language.higherKinds

/* A category consists of objects and arrows `==>` between these objects. An arrow describes a morphism between to
 * objects. The following laws have to be fulfilled:
 *    1. each object has an identity arrow `A ==> A`
 *    2. arrows compose
 *    3. that composition is assocciative
 * 
 * Here, our objects a all types and the arrows are described by a higher-kinded type `==>[_, _]`
 */
trait Category[==>[_, _]] {

  def identity[A]: A ==> A

  def compose[A, B, C](f: A ==> B, g: B ==> C): A ==> C
}

object Category {

  @inline def apply[==>[_, _]: Category] = implicitly[Category[==>]]

  implicit object Function1Category extends Category[Function1] {

    def identity[A]: A => A = a => a

    def compose[A, B, C](f: A => B, g: B => C): A => C = g compose f
  }
}





/* Let's continue with operations on categories. 
 * 
 * The first operation is a mapping of objects and arrows from one category into another. That
 * transformation is known as Functor and follows the laws:
 *     1. associate to each object x in category C in object F[x] in category D
 *     2. associate to each arrow `A ==> B` in C and arrow in D `F[A] ~~> F[B]` and obey:
 *       a. F[A ==> A]     == F[A] ==> F[A]
 *       b. F[g compose f] == F[g] compose F[f]
 */
abstract class Functor[==>[_, _]: Category, ~~>[_, _]: Category, F[_]] {

  def map[A, B](f: A ==> B): F[A] ~~> F[B]
}

abstract class EndoFunctor[==>[_, _]: Category, F[_]] extends Functor[==>, ==>, F]

abstract class ScalaFunctor[F[_]] extends EndoFunctor[Function1, F]

// Let's have an implementation example to see how it works:
object ScalaFunctor {

  implicit object OptionScalaFunctor extends ScalaFunctor[Option] {

    def map[A, B](f: A => B): Option[A] => Option[B] = {
      case Some(a) => Some(f(a))
      case None    => None
    }
  }
}





abstract class BiFunctor[==>[_, _]: Category, ~~>[_, _]: Category, -->[_, _]: Category, F[_, _]] {

  def bimap[A, B, C, D](f: A ==> C, g: B ~~> D): F[A, B] --> F[C, D]
}

abstract class ScalaBiFunctor[F[_, _]] extends BiFunctor[Function1, Function1, Function1, F]





/* A Functor is covariant - meaning you describe a mapping A ==> B on F[A]. But what if we need a contravariant Functor where
 * the mapping is inverted: B ==> A on F[A]?
 * 
 * To achieve that we need the opposite of a category which basically inverts the direction of all arrows.
 */
case class Opposite[==>[_, _]: Category, A, B](run: B ==> A)

object Opposite {

  implicit def oppositeCat[==>[_, _]: Category] = new Category[Opposite[==>, ?, ?]] {

    def identity[A]: Opposite[==>, A, A] = Opposite(Category[==>].identity)

    def compose[A, B, C](f: Opposite[==>, A, B], g: Opposite[==>, B, C]): Opposite[==>, A, C] = Opposite(Category[==>].compose(g.run, f.run))
  }
}

abstract class Presheaf[==>[_, _]: Category, F[_]] extends Functor[Opposite[==>, ?, ?], ==>, F]
// def map[A, B](f: Opposite[==>, A, B]): F[A] ==> F[B]
//   '-> def map[A, B](f: B ==> A): F[A] ==> F[B]

abstract class Contravariant[==>[_, _]: Category, F[_]] extends Presheaf[==>, F]

abstract class ScalaContravariant[F[_]] extends Contravariant[Function1, F]





/* Of course, now you can also have a construct like a ProFunctor which combines a contravariant and covariant Functor. */
abstract class ProFunctor[==>[_, _]: Category, ~~>[_, _]: Category, F[_, _]] extends BiFunctor[Opposite[==>, ?, ?], ~~>, Function1, F]

abstract class HomFunctor[==>[_, _]: Category, F[_, _]] extends ProFunctor[==>, ==>, F]

abstract class ScalaProFunctor[F[_, _]] extends HomFunctor[Function1, F]





abstract class ApplicativeFunctor[==>[_, _]: Category, ~~>[_, _]: Category, F[_]] extends Functor[==>, ~~>, F] {

  def pure[A]: A ==> F[A]

  def ap[A, B](f: F[A ==> B]): F[A] ~~> F[B]
}

abstract class ApplicativeEndoFunctor[==>[_, _]: Category, F[_]] extends ApplicativeFunctor[==>, ==>, F]

abstract class ScalaApplicative[F[_]] extends ApplicativeEndoFunctor[Function1, F]






abstract class Monad[==>[_, _]: Category, F[_]] extends ApplicativeEndoFunctor[==>, F] {

  def join[A](f: F[F[A]]): F[A]
}

abstract class ScalaMonad[F[_]] extends Monad[Function1, F]






case class Kleisli[F[_], A, B](run: A => F[B])

object Kleisli {

  implicit def kleisliCat[F[_]](implicit M: Monad[Function1, F]) = new Category[Kleisli[F, ?, ?]] {

    def identity[A]: Kleisli[F, A, A] = Kleisli(a => M.pure(a))

    def compose[A, B, C](f: Kleisli[F, A, B], g: Kleisli[F, B, C]): Kleisli[F, A, C] = Kleisli { a =>
      M.join(M.map(g.run)(f.run(a)))
    }
  }

  type Id[A] = A

  type Reader[E, A] = Kleisli[Id, E, A]
}

abstract class Traverse[F[_], M[_]](implicit F: Monad[Function1, F]) extends EndoFunctor[({ type K[X, Y] = Kleisli[F, X, Y] })#K, M]
// def map[A, B](f: Kleisli[F, A, B]): Kleisli[F, M[A], M[B]]
//   '-> def map[A, B](f: A => F[B]): M[A] => F[M[B]]
//
// Traverse[List, Future].map[Int, Int](a => Future(a + 1)).run(List(1, 2, 3)) : Future(List(2, 3, 4))

abstract class Monoid[==>[_, _]: Category, A] {

  def empty: A ==> A = Category[==>].identity

  def mappend(f: A ==> A, g: A ==> A): A ==> A = Category[==>].compose(f, g)
}

object Monoid {

  @inline def apply[==>[_, _], A](implicit M: Monoid[==>, A]) = M

  implicit val intPlusMonoid = new Monoid[Function1, Int] {}

  val zero = Monoid[Function1, Int].empty
  val one  = Monoid[Function1, Int].mappend(_ + 1, zero)
  val two  = Monoid[Function1, Int].mappend(one, one)
}
