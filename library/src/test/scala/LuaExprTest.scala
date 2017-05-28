import expr.element.{bool, double, string}
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
}