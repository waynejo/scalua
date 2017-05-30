package expr

import expr.element.core._
import expr.element._

trait LuaExpr[T]

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
case class SDExprIF(condition: LuaExpr[bool], ifTrue: LuaStatement, ifFalse: LuaStatement = SDNone()) extends LuaStatement
case class SDExprAssign[T <: SDType](variable: SDExprVariable[T], expr: LuaExpr[T]) extends LuaStatement
case class SDComposedStatement(one: LuaStatement, another: LuaStatement) extends LuaStatement
case class SDExprReturn[R <: SDType](value: LuaExpr[R]) extends LuaStatement
case class SDExprJust[R <: SDType](value: LuaExpr[R]) extends LuaStatement
case class SDExprValDef[R <: SDType](name: String) extends LuaStatement
case class SDExprBlock(value: List[LuaExpr[Unit]]) extends LuaStatement

sealed trait LuaFunction {
    def name(): String
}

case class SDFunction0(name: String, body: LuaStatement) extends LuaStatement with LuaFunction {
    def apply(): SDExprJust[Unit] = SDExprJust(SDExprFuncCall[Unit](this))
}

case class SDFunction1[R <: SDType](name: String, result: SDExprReturn[R]) extends LuaStatement with LuaFunction {
    def apply(): SDExprFuncCall[R] = SDExprFuncCall[R](this)
}
