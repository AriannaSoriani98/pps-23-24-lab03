package u03.extensionmethods

import u03.Optionals.*
import Optional.*

object Sequences:
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    extension (l: Sequence[Int])
      def sum: Int = l match
        case Cons(h, t) => h + t.sum
        case _          => 0

    extension [A](l: Sequence[A])

      def map[B](mapper: A => B): Sequence[B] = l match
        case Cons(h, t) => Cons(mapper(h), t.map(mapper))
        case Nil()      => Nil()

      def filter(pred: A => Boolean): Sequence[A] = l match
        case Cons(h, t) if pred(h) => Cons(h, t.filter(pred))
        case Cons(_, t)            => t.filter(pred)
        case Nil()                 => Nil()

      def zip[B](second:Sequence[B]): Sequence[(A,B)] = (l,second) match
        case (Cons(h,t),Cons(h2,t2)) => Cons((h,h2), t.zip(t2))
        case _ => Nil()

      def take(n:Int): Sequence[A] = l match
        case Cons(h,t) if n>0 => Cons(h,t.take(n-1))
        case _ => Nil()

      def concat(second:Sequence[A]): Sequence[A] = l match
        case Cons(h,t) => Cons(h,t.concat(second))
        case Nil() => second match
          case Cons(h,t) => Cons(h,l.concat(t))
          case Nil() => Nil()

      def flatMap[B](mapper: A=> Sequence[B]): Sequence[B] = l match
        case Cons(h,t) => mapper(h).concat(t.flatMap(mapper))
        case _ => Nil()


      def foldLeft[B](initialValue: B)(f: (B, A) => B): B = l match
        case Cons(h, t) => t.foldLeft(f(initialValue, h))(f)
        case _ => initialValue


    def of[A](n: Int, a: A): Sequence[A] =
      if (n == 0) then Nil[A]() else Cons(a, of(n - 1, a))



@main def trySequences() =
  import Sequences.*
  import Sequence.*
  
  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  println(seq.filter(_ >= 20).map(_ + 1).sum) // 21+31 = 52
  println(sum(map(filter(seq)(_ >= 20))(_ + 1))) // equally possible
  val seq2 = of(10, -1) // Cons(-1, Cons(-1, Cons(-1, ...)))
  println(seq2.sum) // -10

  println(seq.zip(seq)) //// Cons ((10 ,10), Cons ((20 ,20), Cons ((30 ,30), Nil ())))
  println(seq.take(1)) // Cons ((10, Nil())
  println(seq.concat(seq)) //Cons(10, Cons(20, Cons(30, Cons(10, Cons(20, Cons(30, Nil()))))
  println(seq.flatMap(v => Sequence.Cons(v + 1, Sequence.Cons(v + 2, Sequence.Nil())))) // Cons (11 , Cons (12 , Cons (21 , Cons (22 , Cons (31 , Cons (32 , Nil ()))))))
