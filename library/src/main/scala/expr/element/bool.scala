package expr.element

import expr._
import expr.element.core._

trait bool extends SDType with Andable with Orable with Notable

object bool {
    def apply(value: Boolean): LuaExpr[bool] = SDJustBool(value)
}
