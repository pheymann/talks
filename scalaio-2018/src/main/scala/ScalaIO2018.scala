
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
    ),

    slide(
      "Current State",
      <.p("What we want to end up with:"),
      <.br,
      scalaC("""
        val client: (String, Int) => IO[List[User]] = (name, minAge) => ???
      """)
    ),

    noHeaderSlide(
      <.p("Ho does the HTTP request looks like.")
    ),

    slide(
      "Do a Request",
      scalaC("""
        trait HttpRequest[M, F[_], C, Out] {

          def apply(path: List[String], queries, Map[String, List[String]), client: C): F[Out]
        }
      """),
      scalaCFragment("""

        implicit def http4sIOGet[O] = new HttpRequest[GetMethod, IO, Client[IO], O] {
          ...
        }
      """)
    ),

    slide(
      "What we need",
      Enumeration(
        Item.stable("types of input parameters (segment, query)"),
        Item.fadeIn("literal types of queries"),
        Item.fadeIn("separation of method and return type"),
        Item.fadeIn("list of path elements")
      )
    ),

    slide(
      "What we need",
      <.p("What we want is to fold our API type."),
      <.br,
      scalaC("""
        Get[List[User]] :: MinAgeQ :: Name :: Users :: HNil 

            ~>

        (
          QueryInput   :: SegmentInput :: Users :: HNil, // El
          Int          :: String       :: HNil,          // KIn
          minAgeW.T    :: nameW.T      :: HNil,          // VIn
          GetMethod,                                     // M
          List[User]                                     // Out
        )
      """)
    ),

    noHeaderSlide(
      <.p("But how do you fold a HList type?")
    ),

    noHeaderSlide(
      <.p("How to iterate over a HList type?")
    ),

    slide(
      "Iterate over a HList type",
      <.p("You can leverage Scala's implicit resolution mechanism to iterate over an HList type. Use implicit recursive resolution.")
    ),

    slide(
      "Type-Level Fold Left",
      scalaC("""
        // start with a type-class describing your computation
        trait TplFolder[H <: HList, Agg] { type Out }

        type Aux[H <: HList, Agg, Out0] = TplFolder[H, Agg] { type Out = Out0 }
      """),
      scalaCFragment("""
        // final case
        implicit def returnTplFolder[Agg] = new TypeLevelFoldLeft[HNil, Agg] {
          type Out = Agg
        }
      """),
      scalaCFragment("""
        // recursive resolution (useless for now)
        implicit def recTplFolder[H, T <: HList, Agg, FOut](implicit next: TplFolder.Aux[T, H, FOut]) =
          new TplFolder[H :: T, Agg] { type Out = FOut }
      """)
    ),

    slide(
      "Type-Level Fold Left",
      <.p("Define a mapping case from two types In and Agg to Out."),
      <.br,
      scalaC("""
        trait TplCase[In, Agg] { type Out }

        type Aux[In, Agg, Out0] = TplCase[In, Agg] { type Out = Out0 }
      """),
      scalaCFragment("""
        implicit def queryCase
            [K <: Symbol, V, El <: HList, KIn <: HList, VIn <: HList, M, Out] =
          new TplCase[Query[K, V], (El, KIn, VIn, M, Out)] {
            type Out = (QueryInput :: El, K :: KIn, V :: VIn, M, Out)
          }
 
        ...
      """)
    ),

    slide(
      "Type-Level Fold Left",
      <.p("Define a mapping case from two types In and Agg to Out."),
      <.br,
      scalaC("""
        trait TplCase[In, Agg] { type Out }



        //implicit def queryCase
        //    [K <: Symbol, V, El <: HList, KIn <: HList, VIn <: HList, M, Out] =
          new TplCase[Query[K, V], (El, KIn, VIn, M, Out)] {
            type Out = (QueryInput :: El, K :: KIn, V :: VIn, M, Out)
          }
 
        ...
      """)
    ),

    slide(
      "Type-Level Fold Left",
      scalaC("""
        // recursive resolution (useleful)
        implicit def recTplFolder[H, T <: HList, Agg, COut, FOut]
            (implicit f:    TplCase.Aux[H, Agg, COut],
                      next: Lazy[TplFolder.Aux[T, COut, FOut]]) =
          new TplFolder[H :: T, Agg] { type Out = FOut }
      """)
    ),

    slide(
      "Fold our API",
      scalaC("""
        type Api = Get[List[User]] :: MinAgeQ :: Name :: Users :: HNil

        // (HNil, HNil, HNil, GetCall, List[User])
        recTplFolder(getCase,
          // (QueryInput :: HNil, minAgeW.T :: HNil, Int :: HNil, ...)
          recTplFolder(queryCase,
            // (SegmentInput :: ..., nameW.T :: ..., String :: ..., ...)
            recTplFolder(segmentCase,
              // (userW.T :: ..., ...)
              recTplFolder(pathCase,
                returnCase))))
      """)
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
