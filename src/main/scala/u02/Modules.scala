package u02

import u02.Modules.Person.{matchcourses, matchcourses_improved}
import u03.Sequences
import u03.Sequences.*


object Modules extends App :

  // An ADT: type + module
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

    import u03.Sequences.*
    import Sequence.*

    def matchcourses(l: Sequence[Person]): Sequence[String] = l match
      case Cons(Person.Teacher(_,course),t) => Cons(course,matchcourses(t))
      case Cons(Person.Student(_,_),t) => matchcourses(t)
      case _ => Nil()

    def matchcourses_improved(l: Sequence[Person]): Sequence[String] = flatMap(l)(l=> l match
      case Teacher(_,course) => Cons(course,Nil())
      case _ => Nil()
    )


  println(Person.name(Person.Student("mario", 2015)))
  val l = Sequence.Cons(Person.Student("mario", 2015), Sequence.Cons(Person.Teacher("luca", "PPS"), Sequence.Nil()))
  println(matchcourses(l)) //Cons(PPS),Nil()
  println(matchcourses_improved(l)) //Cons(PPS),Nil()
  import Person.*

  println(name(Student("mario", 2015)))

  // a method outside the Person module
  def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _ => false

  println(isStudent(Student("mario", 2015)))