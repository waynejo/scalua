package expr

import expr.element.core._
import expr.element.{BoolValue, DoubleValue}

trait LuaExpr[T] extends SDValue
case class SDExprVariable[T <: SDType](name:String) extends LuaExpr[T]

case class SDJustDouble(value: Double) extends DoubleValue
case class SDJustBool(value: Boolean) extends BoolValue
case class SDExprAdd(one: Addable, another: Addable) extends DoubleValue
case class SDExprSub(one: Subable, another: Subable) extends DoubleValue
case class SDExprMul(one: Mulable, another: Mulable) extends DoubleValue
case class SDExprDiv(one: Divable, another: Divable) extends DoubleValue
case class SDExprAnd(one: Andable, another: Andable) extends BoolValue
case class SDExprOr(one: Orable, another: Orable) extends BoolValue
case class SDExprNot(value: Notable) extends BoolValue
case class SDExprFuncCall[R <: SDType](value: LuaFunction, args: Vector[SDValue] = Vector()) extends LuaExpr[R]

sealed trait LuaStatement extends LuaExpr[Unit]

case class SDNone() extends LuaStatement
case class SDExprIF(condition: BoolValue, ifTrue: LuaStatement, ifFalse: LuaStatement = SDNone()) extends LuaStatement
case class SDExprAssign[T <: SDType](variable: SDExprVariable[T], expr: LuaExpr[T]) extends LuaStatement
case class SDComposedStatement(one: LuaStatement, another: LuaStatement) extends LuaStatement
case class SDExprReturn[R <: SDType](value: LuaExpr[R]) extends LuaStatement
case class SDExprJust[R <: SDType](value: LuaExpr[R]) extends LuaStatement

sealed trait LuaFunction {
    def name(): String
}

case class SDFunction0(name:String, body:LuaStatement) extends LuaStatement with LuaFunction {
    def apply(): SDExprJust[Unit] = SDExprJust(SDExprFuncCall[Unit](this))
}

case class SDFunction1[R <: SDType](name:String, result:SDExprReturn[R]) extends LuaStatement with LuaFunction {
    def apply(): SDExprFuncCall[R] = SDExprFuncCall[R](this)
}
