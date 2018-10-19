
import SlideshowUtil._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.Style
import org.scalajs.dom

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object ScalaIO extends JSApp {

  import Enumeration._

  val introduction = chapter(
    chapterSlide(
      <.h1("Let's derive HTTP Clients from Types")
    ),

    noHeaderSlide(
      <.h3("Type-level programming")
    ),

    slide(
      "Type-Level Programming",
      <.p("Use types to describe computations which get evalutated by the compiler.")
    ),

    slide(
      "Type-Level Programming: hands-on example",
      <.p("How can we convince the compiler to derive HTTP client functions from an API descriptions?"),
      <.br,
      scalaC("""
        // GET /users/:name?minAge=:age -> List[User]

        (name: String, minAge: Int) => F[List[User]]
      """)
    ),

    slide(
      "What we need",
      <.h2("1"),
      <.p("represent information on the type-level using singleton types"),
    ),

    slide(
      "What we need",
      <.h2("2"),
      <.p("implicit recursion ~ type-level computation")
    ),

    noHeaderSlide(
      <.p("????")
    ),

    slide(
      "Speaker",
      <.h3("Paul Heymann - Data Engineer @ XING")
    ),

    noHeaderSlide(
      <.h2("You have a question?"),
      <.h3("Ask it right away!")
    )
  )

  val apiAsAType = chapter(
    chapterSlide(
      <.h2("API as a Type"),
      <.br,
      <.h3("How to represent information on the Type-Level")
    ),

    slide(
      "Our Task",
      <.p("Represent the following API as a Type:"),
      <.br,
      <.p(font(^.color := "#919191", "GET /users/:name?minAge=:age -> List[User]"))
    ),

    slide(
      "Path to Type",
      <.p(font(^.color := "#919191", "GET /"), font(^.color := "#ce1d25", "users"), font(^.color := "#919191", "/:name?minAge=:age -> List[User]")),      
      <.br,
      scalaC("""
        type Users = "users".type
      """),
      scalaCFragment(
        "// Vanilla Scala (2.12) has no accessable literal singleton types (yet). That's a bummer."
      )
    ),

    noHeaderSlide(
      <.p("shapeless.Witness"),
      <.br,
      scalaC("""
        trait Witness {
          type T

          val value: T
        }
      """)
    ),

    slide(
      "Witness as Literal Singleton Type",
      scalaC("""
        val usersW = Witness("users")

        type Users = usersW.T
      """)
    ),

    slide(
      "Make it more precise",
      <.p("Now we have a type. Let's make it a bit more precise."),
      <.br,
      scalaC("""
        sealed trait Path[P]

        type Users = Path[usersW.T]
      """)
    ),

    noHeaderSlide(
      <.p("That's basically it. Now you know how to get information onto the Type-Level."),
      <.br,
      <.h3(
        Style("textTransform") := "none",
        "Singleton Types"
      )
    ),

    slide(
      "Segment to Type",
      <.p(
        font(^.color := "#919191", "GET /users/:"), font(^.color := "#ce1d25", "name"), font(^.color := "#919191", "?minAge=:age -> List[User]")
      ),
      <.br,
      scalaC("""
        sealed trait Segment[K, V]

        val nameW = Witness("name")

        type Name = Segment[nameW.T, String]
      """)
    ),

    slide(
      "Query to Type",
      <.p(
        font(^.color := "#919191", "GET /users/:name?"), font(^.color := "#ce1d25", "minAge=:age"), font(^.color := "#919191", " -> List[User]")
      ),
      <.br,
      scalaC("""
        sealed trait Query[K, V]

        val minAgeW = Witness("minAge")

        type MinAgeQ = Query[minAgeW.T, Int]
      """)
    ),

    slide(
      "Method to Type",
      <.p(
        font(^.color := "#ce1d25", "GET"), font(^.color := "#919191", " /users/:name?minAge=:age -> "), font(^.color := "#ce1d25", "List[User]")
      ),
      <.br,
      scalaC("""
        sealed trait Get[O]

        type GetUsers = Get[List[User]]
      """)
    ),

    noHeaderSlide(
      <.h4("Put it all together with:"),
      <.br,
      <.h3(
        Style("textTransform") := "none",
        "HList"
      )
    ),

    slide(
      "Make it a Whole",
      <.p(
        font(^.color := "#919191", "GET /users/:name?minAge=:age -> List[User]")
      ),
      <.br,
      scalaC("""
        type Api = GetUsers :: Users :: Name :: MinAgeQ :: HNil
      """)
    ),

    noHeaderSlide(
      <.p("Put I don't want to implement all these Witnesses all the time.")
    ),

    slide(
      "Make it convinient",
      scalaC("""
        val apiT = api(
          method  = Get[List[Users]],
          path    = Root / "users" / Segment[String]("name"),
          queries = Queries add Query[Int]("minAge")
        )

        type Api = apiT.T
      """)
    ),

    slide(
      "Methods",
      <.p("We made Get a type without inhabitants. How can we still pass it to a function?"),
      <.br,
      scalaC("""
        final case class TypeCarrier[T]()
      """),      
      scalaCFragment("""
        def Get[O] = TypeCarrier[Get[O]]()
      """)
    ),

    slide(
      "Paths and Segments",
      <.p("You can use implicitly transformation to derive Witnesses for paths and segments."),
      <.br,
      scalaC("""
        final case class PathBuilder[P <: HList]() {
  
          def /[S](path: Witness.Lt[S]): PathBuilder[S :: P] = 
            PathBuilder()
        }
      """)
    ),

    slide(
      "Paths and Segments",
      <.p("We need some help to get segment types."),
      <.br,
      scalaC("""
        class SegmentHelper[V] {

          def apply[K](kWit: Witness.Lt[K]): TypeCarrier[Segment[K, V]] = 
            TypeCarrier[Segment[K, V]]()
        }

        def Segment[V] = new SegmentHelper[V]
      """),
      scalaCFragment("""
        val nameT = Segment[String]("name")
      """)
    ),

    slide(
      "Paths and Segments",
      scalaC("""
        final case class PathBuilder[P <: HList]() {
  
          ...
          def /[K, V](segment: TypeCarrier[SegmentParam[K, V]]): PathBuilder[SegmentParam[K, V] :: P] = 
            PathListBuilder()
        }

        val Root = PathBuilder[HNil]()
      """)
    ),

    slide(
      "Make it convinient",
      scalaC("""
        def api[M, P <: HList, Q <: HList, Api <: HList]
          (method: M, path: PathBuilder[P], queries: QueryBuilder[Q])
          (implicit prepQP: Prepend.Aux[Q, P, Api]): TypeCarrier[M :: Api] = TypeCarrier()
      """),
      scalaCFragment("""

        api(
          method  = Get[List[Users]],
          path    = Root / "users" / Segment[String]("name"),
          queries = Queries add Query[Int]("minAge")
        )
      """)
    ),

    noHeaderSlide(
      <.p("We got our information onto the Type-Level. Now let's derive the client.")
    )
  )

  val deriveHttpClients = chapter(
    chapterSlide(
      <.h2("Derive Clients"),
      <.br,
      <.h3("Recursively derive a client function.")
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
          apiAsAType,
          deriveHttpClients
        )
      )
    )
    .build

  @JSExport
  override def main(): Unit = {
    Show().renderIntoDOM(dom.document.body)
  }
}
