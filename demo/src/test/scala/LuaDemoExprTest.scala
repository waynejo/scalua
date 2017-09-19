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

    test("call function in another function") {
        assert(LuaPrinter.print(Converter.convert {
            def functionA(a: Double, b: Double): Double = {
                0.0
            }
            def functionB(a: Double, b: Double): Double = {
                functionA(a, b)
            }
        }) ==
            """function functionA(a, b)
              |    return 0.0
              |end
              |function functionB(a, b)
              |    return functionA(a, b)
              |end
              |""".stripMargin.replace("\r\n", "\n"))
    }

    test("use simple operation in function") {
        assert(LuaPrinter.print(Converter.convert{
            def sum(a: Double, b: Double): Double = {
                a + b
            }
            sum(1.0, 2.0)
        }) ==
            """function sum(a, b)
              |    return a + b
              |end
              |sum(1.0, 2.0)""".stripMargin.replace("\r\n", "\n"))
    }
}