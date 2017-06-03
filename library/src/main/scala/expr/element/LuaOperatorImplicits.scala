package expr.element

import expr._

object LuaOperatorImplicits {

    implicit class StringValue(private val one: LuaExpr[string]) {
        def +(another: LuaExpr[string]): LuaExpr[string] = SDExprStrAdd[string](one, another)
    }

    implicit class DoubleValue(private val one: LuaExpr[double]) {
        def +(another: LuaExpr[double]): LuaExpr[double] = SDExprAdd[double](one, another)

        def -(another: LuaExpr[double]): LuaExpr[double] = SDExprSub[double](one, another)

        def *(another: LuaExpr[double]): LuaExpr[double] = SDExprMul[double](one, another)

        def /(another: LuaExpr[double]): LuaExpr[double] = SDExprDiv[double](one, another)
    }


    implicit class BoolValue(private val one: LuaExpr[bool]) {
        def &&(another: LuaExpr[bool]): LuaExpr[bool] = SDExprAnd(one, another)

        def ||(another: LuaExpr[bool]): LuaExpr[bool] = SDExprOr(one, another)

        def unary_!(): LuaExpr[bool] = SDExprNot(one)
    }

    case object implicitBool extends bool
    case object implicitDouble extends double
    case object implicitString extends string

    object ExprImplicits {
        implicit def bool2Bool(x: LuaExpr[bool]): Boolean = true
        implicit def expr2bool(x: LuaExpr[bool]): bool = implicitBool
        implicit def expr2double(x: LuaExpr[double]): double = implicitDouble
        implicit def expr2string(x: LuaExpr[string]): string = implicitString
    }
}
