import expr.element._
import org.scalatest._
import expr._
import expr.element.LuaOperatorImplicits._
import expr.element.LuaOperatorImplicits.BoolImplicits._
import expr.SDExprJust

class LuaDemoExprTest extends FunSuite {

    test("if statement") {
        assert(LuaPrinter.print(Converter.convert(
            if (bool(true) && bool(true)) {
                SDExprJust(bool(true))
            }
        ).asInstanceOf[LuaExpr[Any]]) ==
            """if true and true then
              |    true
              |end
              |""".stripMargin)
    }

    test("if else statement") {
        assert(LuaPrinter.print(Converter.convert(
            if (bool(true) && bool(true)) {
                SDExprJust(bool(true))
            } else {
                SDExprJust(bool(false))
            }
        ).asInstanceOf[LuaExpr[Any]]) ==
            """if true and true then
              |    true
              |else
              |    false
              |end
              |""".stripMargin)
    }

    test("nested if statement") {
        assert(LuaPrinter.print(Converter.convert(
            if (bool(true) && bool(true)) {
                if (bool(false)) {
                    SDExprJust(bool(true))
                }
            }
        ).asInstanceOf[LuaExpr[Any]]) ==
            """if true and true then
              |    if false then
              |        true
              |    end
              |end
              |""".stripMargin)
    }
}