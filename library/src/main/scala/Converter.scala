import expr.{LuaExpr, SDExprReturn, SDFunction2}
import expr.element.core._
import expr.element.double

import scala.reflect.api.Trees

object Converter {
    import scala.language.experimental.macros
    import scala.reflect.macros.whitebox.Context

    def convert[T](code: T): LuaExpr[SDType] = macro convertMacro[LuaExpr[SDType]]

    def convertMacro[T: c.WeakTypeTag](c: Context)(code: c.Expr[Any]): c.Expr[LuaExpr[SDType]] = {
        import c.universe._

        def convertCode(code: c.universe.Tree): (c.universe.Tree, List[c.universe.Tree]) = {

            code match {
                case If(cond, thenp, Literal(Constant(_))) => {
                    (q"SDExprIF(${convertCode(cond)._1}, ${convertCode(thenp)._1})", Nil)
                }
                case If(cond, thenp, elsep) => {
                    (q"SDExprIF(${convertCode(cond)._1}, ${convertCode(thenp)._1}, ${convertCode(elsep)._1})", Nil)
                }
                case ValDef(mods, TermName(name), tpt, Apply(TypeApply(Select(Select(selectName, selectName2), TermName("apply")), args), Nil)) => {
                    (q"SDExprValDef($name)", List(ValDef(mods, TermName(name), tpt, Apply(TypeApply(Select(Select(selectName, selectName2), TermName("apply")), args), List(Literal(Constant(name)))))))
                }
                case ValDef(mods, TermName(name), tpt, Apply(TypeApply(Select(Select(selectName, selectName2), TermName("apply")), args), args2)) => {
                    (q"SDExprValDef($name.name)", List(ValDef(mods, TermName(name), tpt, Apply(TypeApply(Select(Select(selectName, selectName2), TermName("apply")), args), args2))))
                }
                case DefDef(_, TermName(name), _, vparamss:List[List[ValDef]], tpt: Tree, rhs: Tree) => {
                    val functionArgTypes = vparamss.flatMap(x => {
                        x.map { case ValDef(_, TermName(paramName), paramTpt: Tree, _)  =>
                            s"${showCode(paramTpt)}, "
                        }
                    }).mkString + showCode(tpt)
                    val functionArgNames = "\"" + name + "\"" + vparamss.flatMap(x => {
                        x.map { case ValDef(_, TermName(paramName), paramTpt: Tree, _)  =>
                            ", \"" + paramName + "\""
                        }
                    }).mkString
                    val argNum = vparamss.headOption.map(_.length).getOrElse(0)
                    val argVariables = vparamss.flatMap(x => {
                        x.map { case ValDef(_, TermName(paramName), paramTpt: Tree, _)  =>
                            s"val $paramName = SDExprVariable[${showCode(paramTpt)}](" + "\"" + paramName + "\")\n"
                        }
                    }).mkString
                    val returnCode = s"SDExprReturn(${convertCode(rhs)._1})"
                    val codeString = s"val $name = SDFunction$argNum[$functionArgTypes]($functionArgNames, {\n$argVariables$returnCode\n})"
                    (Ident(TermName(name)), List(c.parse(codeString)))
                }
                case Block(exprs, expr) => {
                    val tuples = exprs.map(x => convertCode(x)) :+ convertCode(expr)
                    (Block(tuples.flatMap(_._2), q"SDExprBlock(${tuples.map(_._1)})"), Nil)
                }
                case Apply(Select(Select(Select(Select(Ident(TermName("expr")), TermName("element")), TermName("LuaOperatorImplicits")), TermName("ExprImplicits")), TermName("bool2Bool")), List(tree)) => {
                    (convertCode(tree)._1, Nil)
                }
                case Apply(Select(Select(Select(Select(Ident(TermName("expr")), TermName("element")), TermName("LuaOperatorImplicits")), TermName("ExprImplicits")), TermName("expr2bool")), List(tree)) => {
                    (q"${convertCode(tree)._1}", Nil)
                }
                case Apply(Select(Select(Select(Select(Ident(TermName("expr")), TermName("element")), TermName("LuaOperatorImplicits")), TermName("ExprImplicits")), TermName("expr2double")), List(tree)) => {
                    (q"${convertCode(tree)._1}", Nil)
                }
                case Apply(Select(Select(Select(Select(Ident(TermName("expr")), TermName("element")), TermName("LuaOperatorImplicits")), TermName("ExprImplicits")), TermName("expr2string")), List(tree)) => {
                    (q"${convertCode(tree)._1}", Nil)
                }
                case Apply(Select(Select(Select(Select(Ident(TermName("expr")), TermName("element")), TermName("LuaOperatorImplicits")), TermName("ExprImplicits")), TermName("bool2expr")), List(tree)) => {
                    (q"${convertCode(tree)._1}", Nil)
                }
                case Apply(Select(Select(Select(Select(Ident(TermName("expr")), TermName("element")), TermName("LuaOperatorImplicits")), TermName("ExprImplicits")), TermName("double2expr")), List(tree)) => {
                    (q"${convertCode(tree)._1}", Nil)
                }
                case Apply(Select(Select(Select(Select(Ident(TermName("expr")), TermName("element")), TermName("LuaOperatorImplicits")), TermName("ExprImplicits")), TermName("string2expr")), List(tree)) => {
                    (q"${convertCode(tree)._1}", Nil)
                }
                case Apply(Select(Select(Select(Ident(TermName("expr")), TermName("element")), TermName("LuaOperatorImplicits")), TermName("doubleDummyValue")), List(tree)) => {
                    (q"${convertCode(tree)._1}", Nil)
                }
                case Apply(Select(Select(Select(Ident(TermName("expr")), TermName("element")), TermName("LuaOperatorImplicits")), TermName("doubleDummyValue")), List(tree)) => {
                    (q"${convertCode(tree)._1}", Nil)
                }
                case Apply(Select(Select(Select(Ident(TermName("expr")), TermName("element")), TermName("LuaOperatorImplicits")), TermName("doubleDummyValue")), List(tree)) => {
                    (q"${convertCode(tree)._1}", Nil)
                }
                case Apply(fun, args) => {
                    (Apply(convertCode(fun)._1, args.map(tree => convertCode(tree)._1)), Nil)
                }
                case Select(qualifier, name) => {
                    (Select(convertCode(qualifier)._1, name), Nil)
                }
                case expr => (expr, Nil)
            }
        }

        val result: c.universe.Tree = code match {
            case Expr(expr) => convertCode(c.untypecheck(expr))._1
        }
        c.Expr[LuaExpr[SDType]](result)
    }
}
