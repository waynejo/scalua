import expr._

import scala.reflect.api.Trees
import scala.reflect.internal.Flags

object Converter {
    import scala.language.experimental.macros
    import scala.reflect.macros.whitebox.Context

    def convert[T](code: T): LuaExpr = macro convertMacro[LuaExpr]

    def convertMacro[T: c.WeakTypeTag](c: Context)(code: c.Expr[Any]): c.Expr[LuaExpr] = {
        import c.universe._

        def convertCode(code: c.universe.Tree): c.universe.Tree = {

            code match {
                case Literal(Constant(())) =>
                    c.parse(s"""LuaEmptyTree()""")
                case Literal(Constant(value)) if c.Expr[Any](c.typecheck(code)).actualType.typeSymbol.fullName == "scala.Int" =>
                    c.parse(s"""LuaDoubleConstant($value)""")
                case Literal(Constant(value)) if c.Expr[Any](c.typecheck(code)).actualType.typeSymbol.fullName == "scala.Float" =>
                    c.parse(s"""LuaDoubleConstant($value)""")
                case Literal(Constant(value)) if c.Expr[Any](c.typecheck(code)).actualType.typeSymbol.fullName == "scala.Double" =>
                    c.parse(s"""LuaDoubleConstant($value)""")
                case EmptyTree =>
                    c.parse(s"""LuaEmptyTree()""")
                case Super(_, _) =>
                    c.parse(s"""LuaIdent("super")""")
                case This(_) =>
                    c.parse(s"""LuaIdent("this")""")
                case ClassDef(mods, name, tparams, Template(parents, selfType, body)) =>
                    c.parse(s"""LuaClassDef("$name", List(${body.map(convertCode(_)).mkString(", ")}))""")
                case DefDef(Modifiers(mod), TermName(name), _, _, _, _) if 0 != (Integer.parseInt(mod._1.toString) & scala.reflect.internal.Flags.ACCESSOR) =>
                    c.parse(s"""LuaEmptyTree()""")
                case DefDef(mods, TermName(name), tparams, List(vparamss), tpt, rhs) =>
                    c.parse(s"""LuaDef("$name", List(${vparamss.map(convertCode(_)).mkString(", ")}), ${convertCode(rhs)})""")
                case DefDef(mods, TermName(name), tparams, List(), tpt, rhs) =>
                    c.parse(s"""LuaDef("$name", List(), ${convertCode(rhs)})""")
                case Literal(Constant(value: Boolean)) =>
                    c.parse(s"""LuaBoolConstant($value)""")
                case Ident(TermName(name)) =>
                    c.parse(s"""LuaIdent("$name")""")
                case If(cond, thenp, Literal(Constant(_))) =>
                    c.parse(s"LuaIf(${convertCode(cond)}, ${convertCode(thenp)}, LuaEmptyTree())")
                case If(cond, thenp, elsep) =>
                    c.parse(s"LuaIf(${convertCode(cond)}, ${convertCode(thenp)}, ${convertCode(elsep)})")
                case ValDef(mods, TermName(name), tpt, value) =>
                    c.parse(s"""LuaValDef("$name", ${convertCode(value)})""")
                case Assign(lhs, rhs) =>
                    c.parse(s"LuaAssign(${convertCode(lhs)}, ${convertCode(rhs)})")
                case Apply(fun, args) =>
                    c.parse(s"LuaApply(${convertCode(fun)}, List(${args.map(convertCode(_)).mkString(", ")}))")
                case Select(qualifier, name) =>
                    if (qualifier.symbol.isModule) {
                        c.parse(s"""LuaStaticSelect(${convertCode(qualifier)}, "$name")""")
                    } else {
                        c.parse(s"""LuaSelect(${convertCode(qualifier)}, "$name")""")
                    }
                case Block(exprs, expr) =>
                    c.parse(s"LuaBlock(List(${exprs.map(convertCode(_)).mkString(", ")}), ${convertCode(expr)})")
                case _ =>
                    code
            }
        }

        val result: c.universe.Tree = code match {
            case Expr(expr) => convertCode(c.untypecheck(expr))
        }
        c.Expr[LuaExpr](result)
    }
}
