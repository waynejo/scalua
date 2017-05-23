package expr.element

import expr._
import expr.element.core._

trait bool extends SDType

trait BoolValue extends SDValue with LuaExpr[bool] with Andable with Orable with Notable {
    def && (another:BoolValue): BoolValue = SDExprAnd(this, another)
    def || (another:BoolValue): BoolValue = SDExprOr(this, another)
    def unary_! (): BoolValue = SDExprNot(this)
}

object BoolValue {
    implicit def BoolValue2bool(x: BoolValue): Boolean = true
}

object bool {
    def apply(value: Boolean): BoolValue = SDJustBool(value)
}
