import expr._
import expr.element.BoolValue
import expr.element.core.{SDType, SDValue}

object LuaPrinter {

    def addIndent(text: String): String = {
        text.split("\n").map("    " + _).mkString("\n") + "\n"
    }

    def print[T](expr: SDValue): String = {
        expr match {
            case luaExpr:LuaExpr[T] => {
                luaExpr match {
                    case SDExprVariable(name:String) => s"$name"
                    case SDJustDouble(value) => s"$value"
                    case SDJustBool(value) => s"$value"
                    case SDExprAdd(one, another) => s"${print(one)} + ${print(another)}"
                    case SDExprSub(one, another) => s"${print(one)} - ${print(another)}"
                    case SDExprMul(one, another) => s"${print(one)} * ${print(another)}"
                    case SDExprDiv(one, another) => s"${print(one)} / ${print(another)}"
                    case SDExprAnd(one, another) => s"${print(one)} and ${print(another)}"
                    case SDExprOr(one, another) => s"${print(one)} or ${print(another)}"
                    case SDExprNot(value) => s"not ${print(value)}"
                    case SDExprFuncCall(value: LuaFunction, args: Vector[SDValue]) => s"${value.name()}(${args.map(print).mkString(", ")})"
                    case SDNone() => ""
                    case SDExprIF(condition: BoolValue, ifTrue: LuaStatement, ifFalse: SDNone) => s"if ${print(condition)} then\n${addIndent(print(ifTrue))}end\n"
                    case SDExprIF(condition: BoolValue, ifTrue: LuaStatement, ifFalse: SDExprIF) => s"if ${print(condition)} then\n${addIndent(print(ifTrue))}else${print(ifFalse)}\n"
                    case SDExprIF(condition: BoolValue, ifTrue: LuaStatement, ifFalse: LuaStatement) => s"if ${print(condition)} then\n${addIndent(print(ifTrue))}else\n${addIndent(print(ifFalse))}end\n"
                    case SDExprAssign(variable:SDExprVariable[SDType], expr:LuaExpr[T]) => s"${print(variable)} = ${print(expr)}\n"
                    case SDExprReturn(value) => s"return ${print(value)}\n"
                    case SDExprJust(value: LuaExpr[T]) => print(value)
                    case SDFunction0(name:String, body:LuaStatement) => s"function $name()\n${addIndent(print(body))}end\n"
                    case SDFunction1(name:String, result:SDExprReturn[SDType]) => s"function $name()\n${addIndent(print(result))}end\n"
                    case _ => "Error : " + luaExpr
                }
            }
            case _ => "Error2" + expr
        }
    }

    def asString(expr: LuaStatement): String = {
        print(expr)
    }
}
