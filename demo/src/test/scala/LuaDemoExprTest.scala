import expr.element._
import org.scalatest._
import expr._
import expr.element.LuaOperatorImplicits._
import expr.element.LuaOperatorImplicits.ExprImplicits._
import expr.SDExprJust

class LuaDemoExprTest extends FunSuite {

    test("if statement") {
        assert(LuaPrinter.print(Converter.convert(
            if (bool(true) && bool(true)) {
                bool(true)
            }
        )) ==
            """if true and true then
              |    true
              |end
              |""".stripMargin.replace("\r\n", "\n"))
    }

    test("if else statement") {
        assert(LuaPrinter.print(Converter.convert(
            if (bool(true) && bool(true)) {
                bool(true)
            } else {
                bool(false)
            }
        )) ==
            """if true and true then
              |    true
              |else
              |    false
              |end
              |""".stripMargin.replace("\r\n", "\n"))
    }

    test("nested if statement") {
        assert(LuaPrinter.print(Converter.convert(
            if (bool(true) && bool(true)) {
                if (bool(false)) {
                    bool(true)
                }
            }
        )) ==
            """if true and true then
              |    if false then
              |        true
              |    end
              |end
              |""".stripMargin.replace("\r\n", "\n"))
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
              |""".stripMargin.replace("\r\n", "\n"))
    }

    test("define Function") {
        assert(LuaPrinter.print(Converter.convert{
            def myCustomFunc(a: double, b: double): double = {
                double(0.0)
            }
            double(0)
        }) ==
            """function myCustomFunc(a, b)
              |    return 0.0
              |end
              |0.0""".stripMargin.replace("\r\n", "\n"))
    }
}