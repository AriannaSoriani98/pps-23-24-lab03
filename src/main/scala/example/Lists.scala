package example


object Lists:
    
    //ADT List
    enum List[A]:
        case Cons(head:A, tail:List[A])
        case Nil()

    object List:
        def empty[A]():List[A] = Nil()

        def foldLeft(l:List[Int])(default: Int)(op:(Int,Int)=>Int) : Int = l match
          case Cons(h,t) => foldLeft(t)(op(default,h))(op)
          case _ => default

@main def TestList=
        import Lists.* 

        val list1 = List.Cons(10,List.Cons(20,List.Nil()))
        val emptyList = List.Nil()
        println(list1)
        println(List.foldLeft(list1)(0)(_-_)) // -30



