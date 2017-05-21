package expr.element

import expr._
import expr.element.core._

trait double extends SDType

trait DoubleValue extends SDValue with LuaExpr[double] with Addable with Subable with Mulable with Divable {
    def + (another:DoubleValue): DoubleValue = SDExprAdd(this, another)
    def - (another:DoubleValue): DoubleValue = SDExprSub(this, another)
    def * (another:DoubleValue): DoubleValue = SDExprMul(this, another)
    def / (another:DoubleValue): DoubleValue = SDExprDiv(this, another)
}

object double {
    def apply(value: Double): DoubleValue = SDJustDouble(value)
    def apply(value: Int): DoubleValue = SDJustDouble(value)
}
