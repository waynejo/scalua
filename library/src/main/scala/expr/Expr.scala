package expr

import expr.element.core._
import expr.element._

trait LuaExpr[+T]

case class SDExprVariable[T <: SDType](name: String) extends LuaExpr[T]
case class SDJustDouble(value: Double) extends LuaExpr[double]
case class SDJustString(value: String) extends LuaExpr[string]
case class SDJustBool(value: Boolean) extends LuaExpr[bool]
case class SDExprAdd[T <: Addable](one: LuaExpr[T], another: LuaExpr[T]) extends LuaExpr[T]
case class SDExprSub[T <: Subable](one: LuaExpr[T], another: LuaExpr[T]) extends LuaExpr[T]
case class SDExprMul[T <: Mulable](one: LuaExpr[T], another: LuaExpr[T]) extends LuaExpr[T]
case class SDExprDiv[T <: Divable](one: LuaExpr[T], another: LuaExpr[T]) extends LuaExpr[T]
case class SDExprAnd[T <: Andable](one: LuaExpr[T], another: LuaExpr[T]) extends LuaExpr[T]
case class SDExprOr[T <: Orable](one: LuaExpr[T], another: LuaExpr[T]) extends LuaExpr[T]
case class SDExprNot[T <: Notable](value: LuaExpr[T]) extends LuaExpr[T]
case class SDExprStrAdd[T <: string](one: LuaExpr[T], another: LuaExpr[T]) extends LuaExpr[T]
case class SDExprFuncCall[T <: SDType](value: LuaFunction, args: Vector[LuaExpr[SDType]] = Vector()) extends LuaExpr[T]


sealed trait LuaStatement extends LuaExpr[Unit]

case class SDNone() extends LuaStatement
case class SDExprIF(condition: LuaExpr[bool], ifTrue: LuaExpr[SDType], ifFalse: LuaExpr[SDType] = SDNone()) extends LuaStatement
case class SDExprAssign[T <: SDType](variable: SDExprVariable[T], expr: LuaExpr[T]) extends LuaStatement
case class SDExprReturn[T <: SDType](value: LuaExpr[T]) extends LuaStatement
case class SDExprJust[T <: SDType](value: LuaExpr[T]) extends LuaStatement
case class SDExprValDef(name: String) extends LuaStatement
case class SDExprBlock(value: List[LuaExpr[SDType]]) extends LuaStatement

sealed trait LuaFunction {
    def name(): String
}

case class SDVoidFunction(name: String, body: LuaStatement) extends LuaExpr[Unit] with LuaFunction {
    def apply(): SDExprJust[Unit] = SDExprJust(SDExprFuncCall[Unit](this))
}

case class SDFunction0[R <: SDType](name: String, result: SDExprReturn[R]) extends LuaExpr[R] with LuaFunction {
    def apply(): SDExprFuncCall[R] = SDExprFuncCall[R](this)
}

case class SDFunction1[T0 <: SDType, R <: SDType](name: String, arg0:String, result: SDExprReturn[R]) extends LuaExpr[R] with LuaFunction {
    def apply(arg0: LuaExpr[T0]): SDExprFuncCall[R] = SDExprFuncCall[R](this, Vector(arg0))
}

case class SDFunction2[T0 <: SDType, T1 <: SDType, R <: SDType](name: String, arg0:String, arg1:String, result: SDExprReturn[R]) extends LuaExpr[R] with LuaFunction {
    def apply(arg0: LuaExpr[T0], arg1: LuaExpr[T0]): SDExprFuncCall[R] = SDExprFuncCall[R](this, Vector(arg0, arg1))
}