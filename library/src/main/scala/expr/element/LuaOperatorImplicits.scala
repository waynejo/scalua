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

    object BoolImplicits {
        implicit def BoolValue2bool(x: LuaExpr[bool]): Boolean = true
    }
}
