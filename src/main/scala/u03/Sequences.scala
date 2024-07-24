package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u03.Sequences.Sequence
import u03.Sequences.Sequence.Cons

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = first match
      case Cons(h,t) => second match
        case Cons(h2,t2) => Cons((h,h2), zip(t,t2))
        case _ => Nil()
      case _=> Nil()

    /*
    def zip[A, B](l: Sequence[A], r: Sequence[B]): Sequence[(A, B)] = (l, r) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case _ => Nil()
    */

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h,t) if n>0 => Cons(h,take(t)(n-1))
      case _ => Nil()


    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Cons(h,t) => Cons(h,concat(t,l2))
      case Nil() => l2 match
        case Cons(h,t) => Cons(h,concat(l1,t))
        case _ => Nil()


    /*
    def concat[A](l: Sequence[A], r: Sequence[A]): Sequence[A] = l match
      case Cons(h, t) => Cons(h, concat(t, r))
      case _ => r
     */

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(h,t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()

    import u03.Optionals.*
    import Optional.*

    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(h,t)  => min(t) match
        case Just(minimo) if minimo<h => Just(minimo)
        case _ => Just(h)
      case _ => Empty()

@main def trySequences =
  import Sequences.* 
  val l1 = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  val l2 = Sequence.Cons("a", Sequence.Cons("b", Sequence.Cons("c", Sequence.Nil())))

  println(Sequence.sum(l1)) // 30
  println(Sequence.take(l1)(2)) // Cons(10, Cons(20,Nill()))
  println(Sequence.zip(l1,l2)) //// Cons ((10 ,a), Cons ((20 ,b), Cons ((30 ,c), Nil ())))
  println(Sequence.concat(l1,l1)) // Cons(10, Cons(20,Cons(30, Cons(10, Cons(20, Cons(30, Nill()))))
  println(Sequence.flatMap(l1)(v=> Sequence.Cons(v+1, Sequence.Nil())))  // Cons (11 , Cons (21 , Cons (31 , Nil ())))
  println(Sequence.flatMap(l1)(v => Sequence.Cons(v + 1, Sequence.Cons(v + 2, Sequence.Nil())))) // Cons (11 , Cons (12 , Cons (21 , Cons (22 , Cons (31 , Cons (32 , Nil ()))))))
  println(Sequence.min(l1)) // Just(10)
  import Sequence.*

  println(sum(map(filter(l1)(_ >= 20))(_ + 1))) // 21+31 = 52
