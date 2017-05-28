import expr.LuaExpr

object Converter {
    import scala.language.experimental.macros
    import scala.reflect.macros.whitebox.Context

    def convert[T](code: T): LuaExpr[Any] = macro convertMacro[LuaExpr[Any]]

    def convertMacro[T: c.WeakTypeTag](c: Context)(code: c.Expr[Any]): c.Expr[LuaExpr[Any]] = {
        import c.universe._

        def convertCode(code: c.universe.Tree): c.universe.Tree = {
            code match {
                case If(cond, thenp, Literal(Constant(_))) => q"SDExprIF(${removeImplicit(cond)}, ${convertCode(thenp)})"
                case If(cond, thenp, elsep) => q"SDExprIF(${removeImplicit(cond)}, ${convertCode(thenp)}, ${convertCode(elsep)})"
                case expr => expr
            }
        }

        def removeImplicit(value: c.universe.Tree): c.universe.Tree = {
            value match {
                case Apply(Select(_, TermName("BoolValue2bool")), List(tree)) => convertCode(tree)
                case tree => tree
            }
        }

        val result = code match {
            case Expr(expr) => convertCode(expr)
        }
        c.Expr[LuaExpr[Any]](result)
    }
}
