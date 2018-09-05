
import SlideshowUtil._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object Lecture extends JSApp {

  import Enumeration._

  val introduction = chapter(
    chapterSlide(
      <.h3("All you ever wanted to know about"),
      <.br,
      <.h1("Recursion")
    ),

    slide(
      "Definition",
      <.p("Solving a problem where the solution depends on solutions to smaller instances of the same problem.")
    ),

    slide(
      "What we will see",
      Enumeration(
        Item.stable("recursive data type"),
        Item.fadeIn("recursive functions"),
        Item.fadeIn("evalutation strategies")
      )
    )
  )

  val recursiveDS = chapter(
    chapterSlide(
      <.h2("Recursive Data Types")
    ),

    slide(
      "In Short",
      <.p("The definition of a data type depends on itself.")
    ),

    slide(
      "Data Type: Nat",
      haskell("""
        data Nat = Succ Nat | Zero
      """),
      haskellFragment("""
        let _3 = Succ (Succ (Succ Zero))
      """)
    ),

    slide(
      "Data Type: List",
      haskell("""
        data List a = Cons a (List a) | Nil
      """),
      haskellFragment("""
        let list = Cons 0 (Cons 1 (Cons 2 Nil))
      """)
    ),

    slide(
      "Data Type: mutual recursion",
      <.p("Two data types are definited in terms of each other.")
    ),

    slide(
      "Data Type: mutual recursion",
      haskell("""
        data Tree a   = Node a (Forest a) | Empty
        data Forest a = Cons (Tree a) (Forest a) | Nil
      """),
      haskellFragment("""
        -- binary tree with values in nodes
        let tree = Node 0 (Cons (Node 1 Nil) (Cons (Node 2 Nil)))
      """)
    ),

    slide(
      "Data Type: theory",
      <.p("$\\lambda\\alpha.T$"),
      <.br,
      <.p("Type variable $\\alpha$ may exists in $T$ and represents the entire type itself.")
    ),

    slide(
      "Data Type: theory for Nat",
      haskell("""
        data Nat = Succ Nat | Zero
      """),
      <.br,
      <.p("$nat = \\lambda\\alpha.1 + \\alpha$")
    ),

    noHeaderSlide(
      <.h3("Now we have recursive data structures"),
      <.br,
      <.h4("But how do we process them?")
    )
  )

  val recursiveFunctions = chapter(
    chapterSlide(
      <.h2("Recursive Functions")
    ),

    slide(
      "In Short",
      <.p("Functions which call themselves.")
    ),

    slide(
      "Function: factorial",
      haskell("""
        fact :: Int -> Int
        fact 1 = 1
        fact n = n * (fact $ n - 1)
      """),
      haskellFragment("""
          fact 3
        --  '- 3 * (fact 2)
        --           '- (2 * fact 1)
        --                    '- 1
      """),
      haskellFragment("""
        fact 3 == 3 * (fact 2)
               == 6 * (fact 1)
               == 6
      """)
    ),

    slide(
      "Kinds",
      Enumeration(
        Item.stable("single/multi recursion"),
        Item.fadeIn("direct/indirect recursion"),
        Item.fadeIn("structural/generative recursion"),
        Item.fadeIn("anonymous recursion")
      )
    ),

    slide(
      "Single Direct Recursion",
      haskell("""
        length :: List a -> Int
        length Cons _ tail = 1 + (length tail)
        length Nil         = 0
      """)
    ),

    slide(
      "Single Direct Recursion",
      haskell("""
        let list = Cons 0 (Cons 1 Nil)

        length list == 1 + (length $ Cons 2 Nil)
                    == 2 + (length Nil)
                    == 2
      """)
    ),

    slide(
      "Multi Direct Recursion",
      haskell("""
        data BTree a = Node (BTree a) (BTree a) | Leaf a

        size :: BTree a -> Int
        size Node left right = (size left) + (size right)
        size Leaf _          = 1
      """)
    ),

    slide(
      "Multi Direct Recursion",
      haskell("""
        let tree = Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

        size tree == (size $ Node (Leaf 0) (Leaf 1)) + (size $ Leaf 2)
                  == (size $ Leaf 0) + (size $ Leaf 1) + (size $ Leaf 2)
                  == 3
      """)
    ),

    slide(
      "Indirect Recursion",
      haskell("""
        even :: Int -> Bool
        even 0 = True
        even n = odd $ n - 1

        odd :: Int -> Bool
        odd 0 = False
        odd n = even $ n - 1
      """)
    ),

    slide(
      "Indirect Recursion",
      haskell("""
        even 3 == odd 2
               == even 1
               == odd 0
               == False
      """)
    ),

    slide(
      "Mutual Recursion",
      <.p("Two functions which are defined in terms of each other are mutual recursive.")
    ),

    noHeaderSlide(
      <.h3("Structural/Generative")
    ),

    slide(
      "Structural Recursion",
      <.p("When you consume a (recursive) data structure which gets smaller with every step, " + 
          "e.g. length of a list. This kind of recursion is guaranteed$^*$ to terminate.")
    ),

    slide(
      "Generative Recursion",
      <.p("Generates a new data structure from its input and continues to work on it. This kind of recursion isn't guaranteed to terminate.")
    ),

    slide(
      "Generative Recursion",
      haskell("""
        replicate :: a -> n -> List a
        replicate _ 0 = Nil
        replicate a n = Cons a $ replicate a (n - 1)
      """)
    ),

    slide(
      "Generative Recursion",
      haskell("""
        forever :: a -> List a
        forever a = Cons a $ forever a
      """)
    ),

    slide(
      "Anonymous Recursion: Y-Combinator",
      <.p("""
        \[\begin{aligned}
        fact & = \lambda f.\lambda n.\,if\,n == 0\,then\,1\,else\,n * f\,(f)\,(n - 1) \\
        \newline
        fact\,(fact)\,2 & = 2 * fact\,(fact)\,(1)     \\
                        & = 2 * 1 * fact\,(fact)\,(0) \\
                        & = 2
        \end{aligned} \]
      """)
    ),

    slide(
      "Anonymous Recursion: Y-Combinator",
      <.p("""
        \[\begin{aligned}
        Y & = \lambda f.(\lambda x.f(\,x\,x))\,(\lambda x.f(\,x\,x))
        \end{aligned} \]
      """)
    ),

    noHeaderSlide(
      <.h3("How does the evaluation strategy influences recursion?")
    )
  )

  val evaluation = chapter(
    chapterSlide(
      <.h2("Evaluation Strategies")
    ),

    slide(
      "Lazy: call-by-name, call-by-need",
      haskell("""
        replicate :: a -> n -> List a
        replicate _ 0 = Nil
        replicate a n = Cons a $ replicate a (n - 1)
      """)
    ),

    slide(
      "Lazy: call-by-name, call-by-need",
      haskell("""
        -- parameters are evaluated when used
        -- stops here
        replicate 0 100 == Cons 0 $ replicate 0 (100 - 1)
      """)
    ),

    slide(
      "Strict: call-by-value",
      scalaC("""
        def replicate[A](a: A, n: Int): List[A] = 
          if (n == 0) Nil
          else        a :: replicate(a, n - 1)
      """)
    ),

    slide(
      "Strict: call-by-value",
      scalaC("""
        replicate(0, 100) == 0 :: replicate(0, 99)
                          == 0 :: 0 :: replicate(0, 98)
                          == ...
      """)
    ),

    slide(
      "Strict: call-by-value",
      <.p("Every recursive step is eagerly evaluated and thus can overflow the stack.")
    ),

    noHeaderSlide(
      <.h3("One solution: tail-recursion")
    ),

    slide(
      "Tail-Recursion",
      <.p("The last expression must be the recursive call!")
    ),

    slide(
      "Tail-Recursion",
      scalaC("""
        @tailrec
        def replicate[A](a: A, n: Int, agg: List[A] = Nil): List[A] = 
          if (n == 0) agg
          else        replicate(a, n - 1, a :: agg)
      """)
    ),

    slide(
      "Tail-Recursion",
      <.p("Such a function can be represented as imperative loop.")
    ),

    slide(
      "Tail-Recursion",
      scalaC("""
        def replicate[A](a: A, n: Int, agg: List[A] = Nil): List[A] = {
          var agg = List.empty[A]

          (0 until n).foreach { _ =>
            agg = a :: agg
          }
          agg
        }
      """)
    ),

    noHeaderSlide(
      <.h3("But what is with multi or indirect recursion?")
    ),

    slide(
      "Trampolining",
      <.p("Make strict evaluation lazy by moving it onto the Heap.")
    ),

    slide(
      "Trampolining: even-odd",
      scalaC("""
        def even(n: Int): Boolean = 
          if (n == 0) true
          else        odd(n - 1)

        def odd(n: Int): Boolean =
          if (n == 0) false
          else        even(n - 1)
      """)
    ),

    slide(
      "Trampolining",
      scalaC("""
        sealed trait Trampoline[A]
        final case class Cont[A](f: => Trampoline[A]) extends Trampoline[A]
        final case class Pure[A](a: A)                extends Trampoline[A]
      """)
    ),

    slide(
      "Trampolining: even-odd",
      scalaC("""
        def even(n: Int): Trampoline[Boolean] = 
          if (n == 0) Pure(true)
          else        Cont(odd(n - 1))

        def odd(n: Int): Trampoline[Boolean] =
          if (n == 0) Pure(false)
          else        Cont(even(n - 1))
      """)
    ),

    slide(
      "Trampolining: even-odd",
      scalaC("""
        @tailrec
        def run[A](tramp: Trampoline[A]): A = tramp match {
          case Cont(f) => run(f)
          case Pure(a) => a
        }

        run(even(3)) == (=> odd(2))
                     == (=> even(1))
                     == (=> odd(0)
                     == false
      """)
    ),

    slide(
      "Trampoline => Monad",
      <.p("Making Trampoline a Monad leads to Free Monads."),
      <.br,
      <.p(
        ^.cls := "fragment fade-in",
        "But this is another topic :)"
      )
    )
  )

  val Show = ScalaComponent
    .builder[Unit]("Slideshow")
    .renderStatic(
      <.div(
        ^.cls := "reveal",
        <.div(
          ^.cls := "slides",
          introduction,
          recursiveDS,
          recursiveFunctions,
          evaluation
        )
      )
    )
    .build

  @JSExport
  override def main(): Unit = {
    Show().renderIntoDOM(dom.document.body)
  }
}
