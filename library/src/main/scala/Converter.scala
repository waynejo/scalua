import expr.LuaExpr
import expr.element.core._

object Converter {
    import scala.language.experimental.macros
    import scala.reflect.macros.whitebox.Context

    def convert[T](code: T): LuaExpr[SDType] = macro convertMacro[LuaExpr[SDType]]

    def convertMacro[T: c.WeakTypeTag](c: Context)(code: c.Expr[Any]): c.Expr[LuaExpr[SDType]] = {
        import c.universe._

        def convertCode(code: c.universe.Tree): (c.universe.Tree, List[c.universe.Tree]) = {

            code match {
                case If(cond, thenp, Literal(Constant(_))) => {
                    (q"SDExprIF(${removeImplicit(cond)}, ${convertCode(thenp)._1})", Nil)
                }
                case If(cond, thenp, elsep) => {
                    (q"SDExprIF(${removeImplicit(cond)}, ${convertCode(thenp)._1}, ${convertCode(elsep)._1})", Nil)
                }
                case ValDef(mods, TermName(name), tpt, Apply(TypeApply(Select(Select(selectName, selectName2), TermName("apply")), args), Nil)) => {
                    (q"SDExprValDef($name)", List(ValDef(mods, TermName(name), tpt, Apply(TypeApply(Select(Select(selectName, selectName2), TermName("apply")), args), List(Literal(Constant(name)))))))
                }
                case ValDef(mods, TermName(name), tpt, Apply(TypeApply(Select(Select(selectName, selectName2), TermName("apply")), args), args2)) => {
                    (q"SDExprValDef($name.name)", List(ValDef(mods, TermName(name), tpt, Apply(TypeApply(Select(Select(selectName, selectName2), TermName("apply")), args), args2))))
                }
                case Block(exprs, expr) => {
                    val tuples = exprs.map(x => convertCode(x)) :+ convertCode(expr)
                    (Block(tuples.flatMap(_._2), q"SDExprBlock(${tuples.map(_._1)})"), Nil)
                }
                case expr => (expr, Nil)
            }
        }

        def removeImplicit(value: c.universe.Tree): c.universe.Tree = {
            value match {
                case Apply(Select(_, TermName("BoolValue2bool")), List(tree)) => convertCode(tree)._1
                case tree => tree
            }
        }

        val result: c.universe.Tree = code match {
            case Expr(expr) => convertCode(c.untypecheck(expr))._1
        }
        c.Expr[LuaExpr[SDType]](result)
    }
}
