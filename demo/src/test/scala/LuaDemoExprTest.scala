import expr.LuaBlock
import org.scalatest._
import expr._

class LuaDemoExprTest extends FunSuite {

    test("if statement") {
        assert(LuaPrinter.print(Converter.convert {
            val x = true
            val y = false
            if (x && y) {
                x
            }
        }) ==
            """local x = true
              |local y = false
              |if x and y then
              |    x
              |end""".stripMargin.replace("\r\n", "\n"))
    }

    test("if else statement") {
        assert(LuaPrinter.print(Converter.convert {
            val x = true
            val y = false
            if (x && y) {
                x
            } else {
                y
            }
        }) ==
            """local x = true
              |local y = false
              |if x and y then
              |    x
              |else
              |    y
              |end""".stripMargin.replace("\r\n", "\n"))
    }

    test("nested if statement") {
        assert(LuaPrinter.print(Converter.convert {
            val x = true
            val y = false
            if (x && y) {
                if (x) {
                    y
                }
            }
        }) ==
            """local x = true
              |local y = false
              |if x and y then
              |    if x then
              |        y
              |    end
              |end""".stripMargin.replace("\r\n", "\n"))
    }


    test("define function") {
        assert(LuaPrinter.print(Converter.convert {
            def myCustomFunc(a: Float, b: Float): Float = {
                0.0f
            }
        }) ==
            """function myCustomFunc(a, b)
              |    return 0.0
              |end
              |""".stripMargin.replace("\r\n", "\n"))
    }
}