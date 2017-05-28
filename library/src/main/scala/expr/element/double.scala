package expr.element

import expr._
import expr.element.core._

trait double extends SDType with Addable with Subable with Mulable with Divable

object double {
    def apply(value: Double): LuaExpr[double] = SDJustDouble(value)
    def apply(value: Int): LuaExpr[double] = SDJustDouble(value)
}
