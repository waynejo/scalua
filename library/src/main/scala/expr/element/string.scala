package expr.element

import expr._
import expr.element.core._

trait string extends SDType with Addable

object string {
    def apply(value: String): LuaExpr[string] = SDJustString(value)
}
