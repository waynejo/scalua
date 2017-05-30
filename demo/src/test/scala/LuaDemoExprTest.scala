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
        )) ==
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
        )) ==
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
        )) ==
            """if true and true then
              |    if false then
              |        true
              |    end
              |end
              |""".stripMargin)
    }

    test("multiline statement and Variable") {
        assert(LuaPrinter.print(Converter.convert{
            val value = Var[double]()
            value := double(1.0)
            value := double(2.0) + double(3.0) + value
        }) ==
            """local value
              |value = 1.0
              |value = 2.0 + 3.0 + value
              |""".stripMargin)
    }
}