
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
      "Type-Level Programming: What the compiler has to figure out",
      scalaC("""
        // GET /users/:name?minAge=:age -> List[User]

                          \/

        (name: String, minAge: Int) => F[List[User]]
      """)
    ),

    slide(
      "Type-Level Programming: basics",
      <.img(
        ^.src   := "./img/one.svg",
        ^.width := "7%"
      ),
      <.br,
      <.br,
      <.p("Represent information on the type-level using singleton types."),
    ),

    slide(
      "Type-Level Programming: basics",
      <.img(
        ^.src   := "./img/two.svg",
        ^.width := "8%"
      ),
      <.br,
      <.br,
      <.p("implicit recursion ~ type-level computation")
    ),

    noHeaderSlide(
      <.img(
        ^.src   := "./img/lookat.jpg",
        ^.width := "80%",
      )
    ),

    slide(
      "Me: Paul Heymann",
      Enumeration(
        Item.stable("Data Engineer @ Xing"),
        Item.fadeIn("Scala Trainer"),
        Item.fadeIn("infrequently speaking at conferences and Meetups")
      )
    )
  )

  val apiAsAType = chapter(
    chapterSlide(
      <.h2("API as a Type"),
      <.br,
      <.p("How to represent information on the Type-Level")
    ),

    slide(
      "API as a Type",
      <.p(font(^.color := "#919191", "GET /users/:name?minAge=:age -> List[User]"))
    ),

    slide(
      "Path as a Literal-Type",
      <.p(font(^.color := "#919191", "GET /"), font(^.color := "#ce1d25", "users"), font(^.color := "#919191", "/:name?minAge=:age -> List[User]")),      
      <.br,
      scalaC("""
        // not in vanilla Scala 2.12
        type Users = "users".type
      """)
    ),

    slide(
      "Literal-Types with Witness",
      scalaC("""
        trait Witness {
          type T

          val value: T {}
        }
      """)
    ),

    slide(
      "Literal-Types with Witness",
      scalaC("""
        val usersW = Witness("users")

        type Users = usersW.T
      """)
    ),

    slide(
      "Make it more precise",
      scalaC("""
        sealed trait Path[P]

        type Users = Path[usersW.T]
      """)
    ),

    noHeaderSlide(
      <.p("That's basically it. Now you know how to get information onto the Type-Level."),
      <.br,
      <.h4(
        Style("textTransform") := "none",
        "Singleton-Types"
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
        sealed trait Method
        sealed trait Get[O] extends Method

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
      "Create a complete API",
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
      "Create a complete API: v2.0",
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
      "How to carry a Method-Type",
      scalaC("""
        final case class TypeCarrier[T]()

        def Get[O] = TypeCarrier[Get[O]]()
      """)
    ),

    slide(
      "Build up the Paths",
      scalaC("""
        final case class PathBuilder[P <: HList]() {
  
          // implicit conversion
          def /[S](path: Witness.Lt[S]): PathBuilder[S :: P] = 
            PathBuilder()
          
          ...
        }

        def Root = PathBuilder[HNil]
      """),
      scalaCFragment("""
        Root / "users"
      """)
    ),

    slide(
      "Build up the Path: Segment",
      scalaC("""
        val nameT = Segment[String]("name")
      """),
      scalaCFragment("""
        class SegmentHelper[V] {

          def apply[K](kWit: Witness.Lt[K]): TypeCarrier[Segment[K, V]] = 
            TypeCarrier[Segment[K, V]]()
        }

        def Segment[V] = new SegmentHelper[V]
      """)
    ),

    slide(
      "Build up the Path: Segment",
      scalaC("""
        final case class PathBuilder[P <: HList]() {
          ...

          def /[K, V](segment: TypeCarrier[SegmentParam[K, V]]): 
              PathBuilder[SegmentParam[K, V] :: P] = 
            PathListBuilder()
        }
      """),
      scalaCFragment("""
        Root / "users" / Segment[String]("name")
      """)
    ),

    slide(
      "Create a complete API: v2.0",
      scalaC("""
        def api[M <: Method, P <: HList, Q <: HList, Api <: HList]
            (method: M, path: PathBuilder[P], queries: QueryBuilder[Q])
            (implicit prepQP: Prepend.Aux[Q, P, Api], rev: Reverse[Api]): 
            TypeCarrier[M :: rev.Out] = 
          TypeCarrier()
      """),
      scalaCFragment("""

        api(
          method  = Get[List[Users]],
          path    = Root / "users" / Segment[String]("name"),
          queries = Queries add Query[Int]("minAge")
        )
      """)
    )
  )

  val deriveHttpClients = chapter(
    chapterSlide(
      <.h2("Derive Clients"),
      <.br,
      <.p("Recursively derive a client function.")
    ),

    slide(
      "What's next - derive the client",
      scalaC("""
        val client = derive(Api)
 
        // => client: (String, Int) => IO[List[User]]
      """)
    ),

    noHeaderSlide(
      <.p("What information do we need?")
    ),

    slide(
      "Necessary information",
      scalaC("""
        val client = derive[Api]
 
        // => client: (String, Int) => IO[List[User]]
      """),
      Enumeration(
        Item.stable("types of input parameters (segment, query)"),
        Item.stable("return type")
      )
    ),

    noHeaderSlide(
      <.p("But that is not enough to describe a request.")
    ),

    slide(
      "I am a side-effect",
      scalaC("""
        trait HttpRequest[M <: Method, F[_], C, Out] {

          def apply(path: List[String], 
                    queries, Map[String, List[String]), client: C): 
            F[Out]
        }
      """)
    ),

    slide(
      "Necessary information",
      Enumeration(
        Item.stable("types of input parameters (segment, query)"),
        Item.fadeIn("literal types of queries"),
        Item.fadeIn("separation of method and return type"),
        Item.fadeIn("list of path elements")
      )
    ),

    slide(
      "Extract that information",
      scalaC("""
        Get[List[User]] :: Users :: Name :: MinAgeQ :: HNil 

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
      <.p("But how do you transform a HList type?")
    ),

    slide(
      "Iterating over a HList type",
      <.p("You can leverage Scala's implicit resolution mechanism to iterate over an HList type. Use implicit recursive resolution.")
    ),

    slide(
      "Type-Level fold",
      scalaC("""
        // start with a type-class describing your computation
        trait TplFolder[H <: HList, Agg] { type Out }

        type Aux[H <: HList, Agg, Out0] = TplFolder[H, Agg] { type Out = Out0 }
      """),
      scalaCFragment("""
        // final case
        implicit def returnTplFolder[Agg] = new TplFolder[HNil, Agg] {
          type Out = Agg
        }
      """),
      scalaCFragment("""
        // recursive resolution (useless for now)
        implicit def recTplFolder[H, T <: HList, Agg, FOut]
            (implicit next: TplFolder.Aux[T, H, FOut]) =
          new TplFolder[H :: T, Agg] { type Out = FOut }
      """)
    ),

    slide(
      "Type-Level fold: case",
      scalaC("""
        trait TplCase[In, Agg] { type Out }

        type Aux[In, Agg, Out0] = TplCase[In, Agg] { type Out = Out0 }
      """),
      scalaCFragment("""
        implicit def getCase[Out] =
          new TplCase[Get[Out], Unit] {
            type Out = (HNil, HNil, HNil, GetCall, Out)
          }
      """)
    ),

    slide(
      "Type-Level fold: query",
      scalaC("""
        implicit def queryCase
            [K <: Symbol, V, El <: HList, KIn <: HList, VIn <: HList, M, Out] =
          new TplCase[Query[K, V], (El, KIn, VIn, M, Out)] {
            type Out = (QueryInput :: El, K :: KIn, V :: VIn, M, Out)
          }
      """)
    ),

    slide(
      "Type-Level fold: query",
      scalaC("""
        //implicit def queryCase
        //    [K <: Symbol, V, El <: HList, KIn <: HList, VIn <: HList, M, Out] =
          new TplCase[Query[K, V], (El, KIn, VIn, M, Out)] {
            type Out = (QueryInput :: El, K :: KIn, V :: VIn, M, Out)
          }
      """)
    ),

    slide(
      "Type-Level fold",
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
        implicitly[TplFolder[
          Get[List[User]] :: MinAgeQ :: Name :: Users :: HNil, 
          Unit
        ]]

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
    ),

    noHeaderSlide(
      <.p("We have the information we need. Now we will collect all data needed to do a request.")
    ),

    slide(
      "Derive a function to collect request data",
      scalaC("""
        type PathB   = List[String]
        type Queries = Map[String, List[String]]

        trait RequestDataBuilder[El <: HList, 
                                 KIn <: HList, 
                                 VIn <: HList] {

          def apply(inputs: VIn,
                    path: PathB,
                    queries: Queries): (PathB, Queries)
        }
      """)
    ),

    slide(
      "Final implicit case",
      scalaC("""
        implicit val returnBuilder = 
          new RequestDataBuilder[HNil, HNil, HNil] {

            def apply(inputs: HNil, 
                      path: PathB, 
                      queries: Queries): (PathB, Queries) =
              path -> queries
          }
      """)
    ),

    slide(
      "Extract query parameter",
      scalaC("""
        implicit def queryBuilder
            [K <: Symbol, V, T <: HList, KIn <: HList, VIn <: HList, M, O]
            (implicit wit:  Witness.Aux[K], 
                      next: RequestDataBuilder[T, KIn, VIn]) =
          new RequestDataBuilder[QueryInput :: T, K :: KIn, V :: VIn] {

            def apply(inputs: V :: VIn, 
                      path: PathB, 
                      queries: Queries): (PathB, Queries) =
              next(
                inputs.tail,
                path,
                queries + (wit.value.name -> List(inputs.head.toString()))
              )
          }
      """)
    ),

    slide(
      "Extract query parameter",
      scalaC("""
        //implicit def queryBuilder
        //    [K <: Symbol, V, T <: HList, KIn <: HList, VIn <: HList, M, O]
        //    (implicit wit:  Witness.Aux[K], 
        //              next: RequestDataBuilder[T, KIn, VIn]) =
          new RequestDataBuilder[QueryInput :: T, K :: KIn, V :: VIn] {

            def apply(inputs: V :: VIn, 
                      path: PathB, 
                      queries: Queries): (PathB, Queries) =
              next(
                inputs.tail,
                path,
                queries + (wit.value.name -> List(inputs.head.toString()))
              )
          }
      """)
    ),

    slide(
      "Derive a function to collect request data",
      scalaC("""
        val builder = implicitly[RequestDataBuilder[El, KIn, VIn]]

        builder("Gandalf" :: 2019 :: HNil, Nil, Map())
          -> next(input, "users" :: path, queries)
            -> next(input.tail, "Gandalf" :: path , queries)
              -> next(input.tail, path, queries + ("minAge" -> List("2019")))
                -> (List("users", "Gandalf"), Map("minAge" -> List("2019")))
      """)
    ),

    noHeaderSlide(
      <.p("Put it all together and we have a HTTP client.")
    ),

    slide(
      "Complete client",
      scalaC("""
        val req = implicitly[
          HttpRequest[GetMethod, IO, Client[IO], List[User]]
        ]

        val client: (String, Int) => IO[List[User]] = (name, age) => {
          val (path, queries) = builder(name :: age :: HNil, Nil, Map())

          req(path, queries, c)
        }
      """)
    ),

    slide(
      "Derive HTTP client function",
      scalaC("""
        def derive
            [Api <: HList, El <: HList, KIn <: HList, VIn <: HList, M, Out, C]
            (api: TypeCarrier[Api])
            (implicit fold:    Lazy[TypeLevelFoldLeft.Aux[Api, Fold], 
                                    (El, KIn, VIn, M, Out)]
                      builder: RequestBuilder[El, KIn, VIn],
                      req:     ApiRequest[M, IO, C, Out])
            : VIn => C => IO[Out] = 
          vin => c => {
            val (path, queries) = builder(vin, Nil, Map())

            req(path, queries, c)
          }
      """)
    ),

    slide(
      "HTTP client function",
      scalaC("""
        val client = derive(Api)

        // we have to provide a HList
        client("Gandalf" :: 2019 :: HNil)(c)
      """)
    ),

    slide(
      "Derive HTTP client function: v2.0",
      scalaC("""
        def derive
        //    [Api <: HList, El <: HList, KIn <: HList, VIn <: HList, M, Out, C]
        //    (api: TypeCarrier[Api])
        //    (implicit fold:    Lazy[TypeLevelFoldLeft.Aux[Api, Fold], 
        //                            (El, KIn, VIn, M, Out)]
        //              builder: RequestBuilder[El, KIn, VIn],
        //              req:     ApiRequest[M, IO, C, Out],
                      vinToFn: FnFromProduct[VIn => C => IO[Out]])
            : vinToFn.Out = vinToFn { input => c =>
          val (path, queries) = builder(input, Nil, Map())

          req(path, queries, c)
        }
      """)
    ),

    slide(
      "Final result",
      scalaC("""
        val apiT = api(
          method  = Get[List[Users]],
          path    = Root / "users" / Segment[String]("name"),
          queries = Queries add Query[Int]("minAge")
        )

        val client = derive(Api)

        client("Gandalf", 2019)(c)
      """)
    ),
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
