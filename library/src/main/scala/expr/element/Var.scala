package expr.element

import expr.{LuaExpr, SDExprAssign, SDExprVariable}
import expr.element.core.SDType

class Var[T <: SDType](val name: String) extends LuaExpr[T] {

    def this() {
        this("unnamed_var")
    }

    def := (another: LuaExpr[T]): SDExprAssign[T] = {
        SDExprAssign[T](SDExprVariable[T](name), another)
    }
}

object Var {
    def apply[T <: SDType](name: String): Var[T] = new Var(name)

    def apply[T <: SDType](): Var[T] = new Var()
}