import expr._

object LuaPrinter {

    def addIndent(text: String): String = {
        text.split("\n").map("    " + _).mkString("\n") + "\n"
    }

    def printDefParams(expr: LuaExpr): String = {
        expr match {
            case LuaValDef(name: String, LuaEmptyTree()) =>
                name
            case LuaValDef(name: String, value: LuaExpr) =>
                s"$name = ${print(value)}"
            case _ =>
                print(expr)
        }
    }

    def printDefBody(expr: LuaExpr): String = {
        expr match {
            case LuaBlock(exprs, expr) =>
                (exprs.map(print) :+ print(expr)).mkString("\n")
            case LuaEmptyTree() =>
                ""
            case _ =>
                s"return ${print(expr)}"
        }
    }

    def print(expr: LuaExpr): String = {
        expr match {
            case LuaEmptyTree() =>
                ""
            case LuaBlock(exprs, lastExpr) =>
                (exprs :+ lastExpr).map(print).mkString("\n")
            case LuaIdent(v0) =>
                s"$v0"
            case LuaBoolConstant(v0) =>
                s"$v0"
            case LuaDoubleConstant(v0) =>
                s"$v0"
            case LuaStringConstant(v0) =>
                s""""${v0.replace("\\", "\\\\").replace("\"", "\\\"")}""""
            case LuaApply(LuaSelect(v0, "$amp$amp"), List(v1)) =>
                s"${print(v0)} and ${print(v1)}"
            case LuaApply(LuaSelect(v0, "$bar$bar"), List(v1)) =>
                s"${print(v0)} or ${print(v1)}"
            case LuaApply(LuaSelect(v0, "$plus"), List(v1)) =>
                s"${print(v0)} + ${print(v1)}"
            case LuaApply(LuaSelect(v0, "$plusString"), List(v1)) =>
                s"${print(v0)} .. ${print(v1)}"
            case LuaApply(LuaSelect(v0, "$minus"), List(v1)) =>
                s"${print(v0)} - ${print(v1)}"
            case LuaApply(LuaSelect(v0, "$times"), List(v1)) =>
                s"${print(v0)} * ${print(v1)}"
            case LuaApply(LuaSelect(v0, "$div"), List(v1)) =>
                s"${print(v0)} / ${print(v1)}"
            case LuaApply(v0, v1:List[LuaExpr]) =>
                s"${print(v0)}(${v1.map(print).mkString(", ")})"
            case LuaSelect(v0, "unary_$bang") =>
                s"not ${print(v0)}"
            case LuaValDef(name: String, LuaEmptyTree()) =>
                s"local $name"
            case LuaValDef(name: String, value: LuaExpr) =>
                s"local $name = ${print(value)}"
            case LuaDef(defName: String, params:List[LuaExpr], body:LuaExpr) =>
                s"""function $defName(${params.map(printDefParams).mkString(", ")})
                   |${printDefBody(body).split("\n").map("    " + _).mkString("\n")}
                   |end""".stripMargin
            case LuaIf(cond, thenp, LuaEmptyTree()) =>
                s"""if ${print(cond)} then
                   |${print(thenp).split("\n").map("    " + _).mkString("\n")}
                   |end""".stripMargin
            case LuaIf(cond, thenp, elsep) =>
                s"""if ${print(cond)} then
                   |${print(thenp).split("\n").map("    " + _).mkString("\n")}
                   |else
                   |${print(elsep).split("\n").map("    " + _).mkString("\n")}
                   |end""".stripMargin
            case _ =>
                "Error : " + expr
        }
    }

    def asString(expr: LuaExpr): String = {
        print(expr)
    }
}
