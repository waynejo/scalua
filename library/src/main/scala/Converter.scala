
object Converter {
    import scala.language.experimental.macros
    import scala.reflect.macros.whitebox.Context

    def convert[T](code: T): Any = macro convertMacro[T]

    def convertMacro[T: c.WeakTypeTag](c: Context)(code: c.Expr[Any]): c.Expr[Any] = {
        import c.universe._

        def removeImplicit(value: c.universe.Tree): c.universe.Tree = {
            value match {
                case Apply(Select(_, TermName("BoolValue2bool")), List(tree)) => tree
                case tree => tree
            }
        }

        val result = code match {
            case Expr(If(cond, thenp, Literal(Constant(_)))) => q"SDExprIF(${removeImplicit(cond)}, $thenp)"
            case Expr(If(cond, thenp, elsep)) => q"SDExprIF(${removeImplicit(cond)}, $thenp, $elsep)"
            case Expr(expr) => expr
        }
        c.Expr(result)
    }
}
