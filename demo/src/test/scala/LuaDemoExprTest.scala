import expr.element._
import org.scalatest._
import expr._
import expr.SDExprJust
import expr.element.core.SDValue

class LuaDemoExprTest extends FunSuite {

    test("if statement") {
        assert(LuaPrinter.print(Converter.convert(
            if (bool(true) && bool(true)) {
                SDExprJust(bool(true))
            }
        ).asInstanceOf[SDValue]) ==
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
        ).asInstanceOf[SDValue]) ==
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
        ).asInstanceOf[SDValue]) ==
            """if true and true then
              |    if false then
              |        true
              |    end
              |end
              |""".stripMargin)
    }
}