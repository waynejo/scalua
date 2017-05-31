import expr._
import expr.element.Var
import expr.element.core.{SDType, Unit}

object LuaPrinter {

    def addIndent(text: String): String = {
        text.split("\n").map("    " + _).mkString("\n") + "\n"
    }

    def print[T](expr: LuaExpr[T]): String = {
        expr match {
            case SDExprVariable(name: String) => s"$name"
            case SDJustDouble(value) => s"$value"
            case SDJustBool(value) => s"$value"
            case SDJustString(value) => "\"" + value + "\""
            case SDExprAdd(one, another) => s"${print(one)} + ${print(another)}"
            case SDExprSub(one, another) => s"${print(one)} - ${print(another)}"
            case SDExprMul(one, another) => s"${print(one)} * ${print(another)}"
            case SDExprDiv(one, another) => s"${print(one)} / ${print(another)}"
            case SDExprAnd(one, another) => s"${print(one)} and ${print(another)}"
            case SDExprOr(one, another) => s"${print(one)} or ${print(another)}"
            case SDExprNot(value) => s"not ${print(value)}"
            case SDExprStrAdd(one, another) => s"${print(one)} .. ${print(another)}"
            case SDExprFuncCall(value: LuaFunction, args: Vector[LuaExpr[SDType]]) => s"${value.name()}(${args.map(print).mkString(", ")})"
            case SDNone() => ""
            case SDExprIF(condition: LuaExpr[bool], ifTrue: LuaStatement, ifFalse: SDNone) => s"if ${print(condition)} then\n${addIndent(print(ifTrue))}end\n"
            case SDExprIF(condition: LuaExpr[bool], ifTrue: LuaStatement, ifFalse: SDExprIF) => s"if ${print(condition)} then\n${addIndent(print(ifTrue))}else${print(ifFalse)}\n"
            case SDExprIF(condition: LuaExpr[bool], ifTrue: LuaStatement, ifFalse: LuaStatement) => s"if ${print(condition)} then\n${addIndent(print(ifTrue))}else\n${addIndent(print(ifFalse))}end\n"
            case SDExprAssign(variable: SDExprVariable[SDType], expr: LuaExpr[_]) => s"${print(variable)} = ${print(expr)}\n"
            case SDExprReturn(value) => s"return ${print(value)}\n"
            case SDExprJust(value: LuaExpr[_]) => print(value)
            case SDExprValDef(name: String) => s"local $name\n"
            case SDExprBlock(value: List[LuaExpr[Unit]]) => value.map(print).mkString("")
            case SDVoidFunction(name: String, body: LuaStatement) => s"function $name()\n${addIndent(print(body))}end\n"
            case SDFunction0(name: String, result: SDExprReturn[_]) => s"function $name()\n${addIndent(print(result))}end\n"
            case SDFunction1(name: String, arg0: String, result: SDExprReturn[_]) => s"function $name($arg0)\n${addIndent(print(result))}end\n"
            case SDFunction2(name: String, arg0: String, arg1: String, result: SDExprReturn[_]) => s"function $name($arg0, $arg1)\n${addIndent(print(result))}end\n"
            case value: Var[_] => value.name
            case _ => "Error : " + expr
        }
    }

    def asString(expr: LuaStatement): String = {
        print(expr)
    }
}
