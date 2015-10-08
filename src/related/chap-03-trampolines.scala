object trampoline {

    // The trampoline pattern provides a way to implement non-tail recursive
    // algorithms as a chain of recursive calls embedded in instances of an
    // abstract data type, exchanging recursive stackframes for heap space.
    
    // Lets take for example the non-tail recursive ackerman function defined
    // and implemented below.  Its definition employs inner recursion so no
    // tail recursive implementation exists, moreover, the execution of the
    // ackerman function spawns into many recursive calls quite quickly:
    // 
    //   scala> ackermann(1,2)
    //   res1: Int = 4
    //
    //   scala> ackermann(2,2)
    //   res2: Int = 7

    //   scala> ackermann(2,4)
    //   res3: Int = 11

    //   scala> ackermann(4,1)
    //   java.lang.StackOverflowError
    //     at .ackermann(<console>:13)
    //     at .ackermann(<console>:13)
    //     ...
    //
    def ackermann(m: Int, n: Int): Int = (m, n) match {
        case (0, _)            => n + 1
        case (_, 0) if (m > 0) => ackermann(m - 1, 1)
        case (_, _)            => ackermann(m - 1, ackermann(m, n - 1))  
    }

    // We would like to implement the ackerman function in a tailrecursive
    // manner, staying close to the original implementation, here is where
    // trampolines come in.  A trampoline is defined as a monoidic abstract
    // data type, parameterized for the result type of a recursive call
    // chain and one central method `result` which executes that chain and
    // returns its result.  Additionally a trampoline defines `map` and
    // `flatMap` methods that can transform the result of a recursive call
    // chain, making it possible to compose tail recurive algorithms or
    // encoding them using for-comprehensions.  The latter one being usefull
    // when implementing algorithms that are defined with inner recursion,
    // such as the ackermann function.
    //
    // The complete definition of a trampoline requires only a few lines
    // of code as shown below.  The implementation is quite subtle, but
    // for the moment don't pay much attention to how the methods are
    // implemented, that will be explained later in more detail, focus on
    // the method signatures instead.
    trait Tramp[+A] {

        // executes the trampoline's call chain, returning its result.
        @scala.annotation.tailrec final def result: A = this match {
            case Done(a)    => a
            case Call(t)    => t().result
            case Cont(s, c) => s match {
                case Done(v)      => c(v).result
                case Call(t)      => t().flatMap(c).result
                case Cont(ss, cc) => ss.flatMap(x => cc(x) flatMap c).result
            }
        }

        // transforms this trampoline with the transformation function given.
        def flatMap[B](f: A => Tramp[B]): Tramp[B] = this match {
            case Cont(s, c) => Cont(s, (x: Any) => c(x) flatMap f)
            case c          => Cont(c, f)
        }

        // transforms the result of this trampoline with the function given. 
        def map[B](f: A => B): Tramp[B] = flatMap(a => Done(f(a)))
    }

    // The central idea around tramolines is to wrap every recurion in a
    // type instance of a trampoline, the easiest instance being the call
    // terminating a recursive chain, containing its result.  We name this
    // instance `Done` and encode it it as an instance of trampoline simply
    // holding the value it returns as a case class parameter.
    case class Done[+A](a: A) extends Tramp[A]

    // Secondly we require a type instance of a trampoline that is able to
    // model a recursive call and defer its execution.  This can be done
    // in scala beautifully with the definition of what is called a _thunk_.
    // A thunk is a function value that takes no input parameters and returns
    // a result, in our case the next trampoline in the recursive call chain.
    case class Call[+A](t: () => Tramp[A]) extends Tramp[A]

    // Lastly, we model as a type instance of a trampoline the definition of
    // a continuation.  Continuations are what makes the trampoline mechanism
    // so usefull when we want to compose recursive algorithms, employing `map`
    // and `flatMap`.  The continuation instance `Cont` holds as a case class
    // parameter a (non-executed) tramopline call chain `s`, and the intented
    // continuation after execution of `s`, taking its result of type `A` which
    // will be passed into a new trampoline call chain `c`, returning type `B`.
    case class Cont[A, +B] (s: Tramp[A], c: A => Tramp[B]) extends Tramp[B]
    

    // The type class instances are not intended to be used directly by the
    // user implementing a non-tail recursive algorithm in a tail recursive
    // manner.  Instead we provide two factory methods modeling a tail call
    // that is done, called `tdone` and one that recurses called `tcall`.
    def tdone[A](a: A)           = Done(a)
    def tcall[A](t: => Tramp[A]) = Call(() => t)


    // With this two factory methods we are able to implement the non-tail
    // recursive ackermann function true to the structure of its definition
    // by explicitly calling `tdone` at the recurion's breaking case, `tcall`
    // whenever a recursive call occurs, and employing the `map` and `flatMap`
    // to factor out the inner recursion in a for-comprehension. 
    def ackermannTramp(m: Int, n: Int): Tramp[Int] = (m, n) match {
        case (0, _)            => tdone(n + 1)
        case (_, 0) if (m > 0) => tcall(ackermannTramp(m - 1, 1))
        case (_, _)            => for {
            inner <- tcall(ackermannTramp(m, n - 1))
            outer <- tcall(ackermannTramp(m - 1, inner))
        } yield outer
    }

    // We can now execute the ackermann algorithm for larger inputs:
    //
    //   scala> ackermannTramp(4,1).result
    //   res1: Int = 65533
    //
    // Execution of this function takes aproximitly 3 minutes on a Macbook
    // Air 2013, but... returns a result without running into a stack overflow.
    //
    // The ackermann function was designed to show the existance of functions
    // that cannot be defined in a non-recursive way.  The usefullness of the
    // function itself is limited, its value grows rapidly, even for small
    // inputs.  For example A(4,2) is an integer of 19,729 decimal digits.  The
    // trampoline mechanism is usefull non the less, functions that can be 
    // defined both recursively and iteratively can differ much in readability.
    // In cases where the recursive function is more idiomatic or more read-
    // able, but the nature of the recursive function can not be implemented
    // in a tail recursive manner, we now have a way to exchange stack frames
    // for heap space, while retaining the structure of the definition in code.

    // All of the above was written as an explanation how to implement non
    // -tail recursive algorithems, such as the ackermann function, employing
    // the trampoline mechanism.  The trampoline and its type instances themself
    // are part of scala's libery, we don't need to code them ourself as an
    // implementation of a trampoline called `TailCals` is available in the 
    // package `scala.util.control`.  You can just import it, but now you know
    // also how it works.
    import scala.util.control.TailCalls._ 
    def ackermannTailRec(m: Int, n: Int): TailRec[Int] = (m, n) match {
        case (0, _)            => done(n + 1)
        case (_, 0) if (m > 0) => tailcall(ackermannTailRec(m - 1, 1))
        case (_, _)            => for {
            inner <- tailcall(ackermannTailRec(m, n - 1))
            outer <- tailcall(ackermannTailRec(m - 1, inner))
        } yield outer
    }


}