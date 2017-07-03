import expr.{SDExprReturn, SDFunction2}
import expr.element.{Var, bool, double, string}
import expr.element.LuaOperatorImplicits._
import org.scalatest._

class LuaExprTest extends FunSuite {

    test("test bool && operation") {
        assert(LuaPrinter.print(bool(true) && bool(false)) == "true and false")
    }

    test("test bool || operation") {
        assert(LuaPrinter.print(bool(true) || bool(false)) == "true or false")
    }

    test("test bool ! operation") {
        assert(LuaPrinter.print(!bool(true)) == "not true")
    }

    test("test double + operation") {
        assert(LuaPrinter.print(double(1.0) + double(2.0)) == "1.0 + 2.0")
    }

    test("test double - operation") {
        assert(LuaPrinter.print(double(1.0) - double(2.0)) == "1.0 - 2.0")
    }

    test("test double * operation") {
        assert(LuaPrinter.print(double(1.0) * double(2.0)) == "1.0 * 2.0")
    }

    test("test double / operation") {
        assert(LuaPrinter.print(double(1.0) / double(2.0)) == "1.0 / 2.0")
    }

    test("test string + operation") {
        assert(LuaPrinter.print(string("Hello ") + string("world")) == "\"Hello \" .. \"world\"")
    }

    test("calling function") {
        val testFunc = SDFunction2[double, double, double]("testFunc", "v0", "v1", SDExprReturn(double(0)))
        assert(LuaPrinter.print(testFunc(double(0) + double(1), testFunc(double(2), double(3)))) == "testFunc(0.0 + 1.0, testFunc(2.0, 3.0))")
    }

    test("define function") {
        assert(LuaPrinter.print(SDFunction2[double, double, double]("testFunc", "v0", "v1", SDExprReturn(double(0) + double(1)))) ==
            """function testFunc(v0, v1)
              |    return 0.0 + 1.0
              |end
              |""".stripMargin)
    }

    test("define table") {
        assert(LuaPrinter.print(
            SDJustTable[string](List(
                string("a") -> string("apple"),
                string("b") -> string("banana")
            ))) ==
            """{["a"] = "apple", ["b"] = "banana"}""".stripMargin)
    }
}