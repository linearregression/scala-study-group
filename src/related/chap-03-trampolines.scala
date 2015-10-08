object trampoline {

    // The trampoline mechanism provides a way to implement non-tail recursive
    // algorithms as a chain of recursive calls embedded in instances of an
    // abstract data type, exchanging recursive stackframes for heap space.
    //
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
    //
    //   scala> ackermann(2,4)
    //   res3: Int = 11
    //
    //   scala> ackermann(4,1)
    //   java.lang.StackOverflowError
    //     at .ackermann(<console>:13)
    //     at .ackermann(<console>:13)
    //     ...
    //
    //  Note: see also: [https://en.wikipedia.org/wiki/Ackermann_function]
    //
    def ackermann(m: Int, n: Int): Int = (m, n) match {
        case (0, _)            => n + 1
        case (_, 0) if (m > 0) => ackermann(m - 1, 1)
        case (_, _)            => ackermann(m - 1, ackermann(m, n - 1))  
    }

    // We would like to implement the ackerman function in a tail recursive
    // manner, staying close to the original definition, here is where
    // trampolines come in.  A trampoline is defined as a monadic abstract
    // data type, parameterized for the result type of a recursive call
    // chain and one central method `result` which executes that chain and
    // returns its result.  Additionally a trampoline defines `map` and
    // `flatMap` methods that can transform the result of a recursive call
    // chain, making it possible to compose tail recurive algorithms or
    // encoding them using for-comprehensions.  This latter use-case being
    // the implementation of algorithms defined with inner recursion, such
    // as the ackermann function.
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

    // The central idea around tramolines is to wrap every recursive call in
    // a type instance of a trampoline, the easiest instance being the call
    // terminating the recursive chain and containing the algorithm's result.
    // We name this instance `Done` and encode it extending trampoline simply
    // by holding the value it returns as a case class parameter.
    case class Done[+A](a: A) extends Tramp[A]

    // Secondly we require a type instance of a trampoline that is able to
    // model a recursive call and defer its execution.  This can be done
    // in scala elegantly with the definition of what is called a _thunk_.
    // A thunk is a function value that takes no input parameters and returns
    // a result, in our case the next recursive call in the call chain.
    case class Call[+A](t: () => Tramp[A]) extends Tramp[A]

    // Lastly, we model as a type instance of a trampoline the definition of
    // a continuation.  Continuations are what makes the trampoline mechanism
    // usefull when we want to compose recursive algorithms, employing `map`
    // and `flatMap`.  The continuation instance `Cont` holds as a case class
    // parameter a (non-executed) tramopline `s` and its continuation `c` after
    // application of `s`. `c` takes the result of `s` as type `A` which
    // will be passed into a new trampoline resulting type `B`.
    case class Cont[A, +B] (s: Tramp[A], c: A => Tramp[B]) extends Tramp[B]
    

    // The type class instances are not intended to be used directly by the
    // user implementing a non-tail recursive algorithm in a tail recursive
    // manner.  Instead we provide two factory methods modeling a tail call
    // that is done, named `tdone` and one that representing a tail recursive
    // call, named `tcall`.
    def tdone[A](a: A)           = Done(a)
    def tcall[A](t: => Tramp[A]) = Call(() => t)


    // With this two factory methods we are able to implement the non-tail
    // recursive ackermann function true to the structure of its definition
    // by explicitly calling `tdone` at the recurion's breaking case and
    // `tcall` whenever a recursive call occurs in our algorithm.  We employing
    // the `map` and `flatMap` methods to factor out the inner recursion in a
    // for-comprehension. 
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
    // Air 2013, but... it returns a result without running into a stack
    // overflow, which for ackermann implementations is quite an achievement.
    //
    // Lets have a look on how the trampoline methods are implemented.
    //
    // Central to the trampoline, and without its monadic `map` and `flatMap`
    // methods lay the type instances, `Done` and `Call`.  Upon execution, the
    // `result` method simply matches the instance, returning the contained
    // result value in the case that the recursion is `Done`, or applying the
    // tail call's thunk from which it returns its result recursively in the
    // case that the instance is a `Call`, a "current step in the chain":
    //
    //   def result: A = this match {
    //       case Done(a)    => a
    //       case Call(t)    => t().result
    //       ...
    //   }
    //
    // The methods `map` and `flapMap` compose the trampoline's monadic design
    // and this is where continuations come into play.  Upon execution of a
    // composed, trampolined recursive algorithm, the `result` method should
    // first execute the left hand side of a continuation, after which it
    // should pass its result as the input parameter to its right hand side.
    // Note that we want to defer execution of the complete composed trampoline
    // up to the moment that the `result` method is being executed at the call-
    // side, which is the reason that we modelled the continuation as a type
    // instance of a trampoline itself, named `Cont`.  We provide for this case
    // with an inner match on `Cont`'s left hand side `s`:
    //
    //   def result: A = this match {
    //       case Done(a)    => a
    //       case Call(t)    => t().result
    //       case Cont(s, c) => s match {
    //           case Done(v)      => c(v).result
    //           case Call(t)      => t().flatMap(c).result
    //           case Cont(ss, cc) => ss.flatMap(x => cc(x) flatMap c).result
    //       }
    //   }    
    // 
    // We continue executing the right hand side `c` in case `s` is `Done`, or
    // flatmap `c` over thunk `t` in case `s` is a `Call` instance itself and
    // execute the result of this composed chain, again recursively.  Both
    // cases are quite straight forward, but what should we do in case `s` is a
    // continuation itself?  Here is where the monadic design of a trampoline
    // comes to the rescue.
    // 
    // Since the left hand side `ss` and the right hand side `cc` of the
    // current continuation value `c` are trampolines, we can compose the
    // complete trampoline by flat-mapping a function over `s` which itself
    // is a composition of the inner continuation `cc` and the outer
    // continuation `c`.  We do this while applying the current recursive step
    // in `Cont`'s left hand side `x`, and then (and only then) call `result`
    // on the composed function, thus defering execution of the application of
    // `x` while the recursive function is composed.  That is pretty nifty, but
    // lets read it again and compare it with the implemetation!
    // 
    // Remaining to be implemented are the monadic methods `map` and
    // `flatMap` themself.  We implement `flatMap` by matching on the current
    // type instance and employ our continuation type `Cont`, simply passing it
    // given transformation function `f` as the continuation, shown in the
    // implementation below as case `c`.
    //
    //   def flatMap[B](f: A => Tramp[B]): Tramp[B] = this match {
    //       case Cont(s, c) => Cont(s, (x: Any) => c(x) flatMap f)
    //       case c          => Cont(c, f)
    //   }
    //
    // A special situation, again, is when the current type instance is a
    // continuation itself, in that case we flatmap given transformation
    // function `f` over the right hand side of the continuation instance `c`,
    // thus composing `f` as "the continuation of the continuation" of the
    // trampoline under construction.
    //
    // Last to be implement is the method `map` which is a trivial case now
    // that we have an implementation for `flatMap`.  We just flatmap a
    // function that takes the result of the current trampoline and return
    // a `Done` instance after application of given transformation function.
    //
    //   def map[B](f: A => B): Tramp[B] = flatMap(a => Done(f(a)))
    //
    //
    // The ackermann function was designed to show the existance of functions
    // that cannot be defined in a non-recursive way.  The usefullness of the
    // function itself is limited, its value grows rapidly, even for small
    // inputs.  For example ackerman(4,2) yields an integer of 19,729 decimal
    // digits.  The trampoline mechanism is usefull non the less, functions
    // that can be defined both recursively and iteratively can differ much in
    // readability.
    //
    // In cases where the recursive function is more idiomatic or more read-
    // able, but the nature of the recursive function can not be implemented
    // in a tail recursive manner, we now have a way to exchange stack frames
    // for heap space, while retaining the structure of the definition in code.
    //
    // All of the above was written as an explanation how to implement non-
    // tail recursive algorhitms such as the ackermann function, employing
    // the trampoline mechanism.  The trampoline and its type instances themself
    // are part of scala's libery, we don't need to code them ourself as an
    // implementation of a trampoline called `TailCalls` is available in the 
    // package `scala.util.control`.  We can just import it and implement
    // our recursively defined algorithm as shown above,...  but now while
    // understanding how things are working internally.
    //
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