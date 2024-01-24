import scala.quoted.*

object Macros {
  inline def typeOf[A]: Doc = ${ typeOfMacro[A] }

  case class Doc(cls: Class[_], description: String) {
    override def toString: String = s"Doc of $cls: $description"
  }

  private def typeOfMacro[A](using tpe: Type[A], quotes: Quotes): Expr[Doc] = {
    import quotes.*
    import quotes.reflect.*
    val t = TypeRepr.of[A]


    def clsOf(t: TypeRepr): Expr[Class[_]] =
      Literal(ClassOfConstant(t)).asExpr.asInstanceOf[Expr[Class[_]]]

    def belongsToScalaDefault(t: TypeRepr): Boolean =
      val scalaDefaultPackages = Seq("scala.", "scala.Predef$.", "scala.util.")
      val nme = t.typeSymbol.fullName
      scalaDefaultPackages.exists(p => nme.startsWith(p))

    def extract(t: TypeRepr): Expr[Class[_]] = {
      println(s"extract ${t.getClass}")
      t match

        case t if t.typeSymbol.isType && t.typeSymbol.isAliasType && !belongsToScalaDefault(t) =>
          val dealiased = t.dealias
          println(s"=== alias factory: ${t} => ${dealiased} == ${t.simplified}")
          extract(dealiased)

        case h: TypeLambda =>
          println(s"TypeLambda $h")
          val len = h.paramNames.size
          val args = Option.when(len > 0)(h.param(0)).map(extract)
          args.getOrElse(clsOf(h.resType))

        case a: AppliedType =>
          println(s"AppliedType")
          val typeArgs = a.args.map(extract)
          typeArgs.head

        case other =>
          println(s"other $other")
          clsOf(other)
    }


    val str = Expr(t.toString)
    '{ Doc( ${extract(t)}, $str)}
  }
}
