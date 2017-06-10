package expr.element

import expr.element.core.SDType
import expr.{LuaExpr, SDJustTable}

class Table[T <: SDType]() extends SDType

object Table {
    def apply[T <: SDType](values: Seq[(LuaExpr[string], LuaExpr[T])]): LuaExpr[Table[T]] = SDJustTable[T](values)
}