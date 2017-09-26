import expr._

object LuaPrinter {
    val INITIALIZER = "<init>"

    def addIndent(text: String): String = {
        text.split("\n").map(s => rtrim("    " + s)).mkString("\n")
    }

    def rtrim(s: String) = s.replaceAll("\\s+$", "")


    def selectingName(name: String): String = {
        name match {
            case INITIALIZER =>
                "__init__"
            case _ =>
                name
        }
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

    def printClassBody(expr: LuaExpr): String = {
        expr match {
            case LuaValDef(name: String, LuaEmptyTree()) =>
                s"class.$name = nil"
            case LuaValDef(name: String, value: LuaExpr) =>
                s"class.$name = ${print(value)}"
            case LuaDef(defName: String, params:List[LuaExpr], body:LuaExpr) =>
                s"""class.${selectingName(defName)} = function(${("this" +: params.map(printDefParams)).mkString(", ")})
                   |${addIndent(printDefBody(body))}
                   |end""".stripMargin
            case _ =>
                print(expr)
        }
    }

    def print(expr: LuaExpr): String = {
        expr match {
            case LuaEmptyTree() =>
                ""
            case LuaBlock(exprs, lastExpr) =>
                (exprs :+ lastExpr).map(print).map(rtrim).mkString("\n")
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
            case LuaApply(LuaSelect(LuaIdent("super"), INITIALIZER), v0) =>
                s"""if super ~= nil then
                   |    ${print(LuaIdent("super"))}:${selectingName(INITIALIZER)}(${v0.map(print).mkString(", ")})
                   |end
                 """.stripMargin
            case LuaApply(LuaSelect(v0, v1), v2:List[LuaExpr]) =>
                s"${print(v0)}:${selectingName(v1)}(${v2.map(print).mkString(", ")})"
            case LuaApply(v0, v1:List[LuaExpr]) =>
                s"${print(v0)}(${v1.map(print).mkString(", ")})"
            case LuaAssign(lhs, rhs) =>
                s"${print(lhs)} = ${print(rhs)}"
            case LuaSelect(v0, "unary_$bang") =>
                s"not ${print(v0)}"
            case LuaStaticSelect(v0, v1) =>
                s"${print(v0)}.${selectingName(v1)}"
            case LuaSelect(v0, v1) =>
                s"${print(v0)}.${selectingName(v1)}"
            case LuaValDef(name: String, LuaEmptyTree()) =>
                s"local $name"
            case LuaValDef(name: String, value: LuaExpr) =>
                s"local $name = ${print(value)}"
            case LuaDef(defName: String, params:List[LuaExpr], body:LuaExpr) =>
                s"""function $defName(${params.map(printDefParams).mkString(", ")})
                   |${addIndent(printDefBody(body))}
                   |end""".stripMargin
            case LuaIf(cond, thenp, LuaEmptyTree()) =>
                s"""if ${print(cond)} then
                   |${addIndent(print(thenp))}
                   |end""".stripMargin
            case LuaIf(cond, thenp, elsep) =>
                s"""if ${print(cond)} then
                   |${addIndent(print(thenp))}
                   |else
                   |${addIndent(print(elsep))}
                   |end""".stripMargin
            case LuaClassDef(name, value) =>
                s"""$name = {}
                   |function $name.__init__()
                   |    class = {}
                   |${addIndent(value.map(printClassBody).filter(_.nonEmpty).mkString("\n"))}
                   |    return class
                   |end
                 """.stripMargin
            case _ =>
                "Error : " + expr
        }
    }

    def asString(expr: LuaExpr): String = {
        print(expr)
    }
}
