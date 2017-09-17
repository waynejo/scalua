
import expr._
import org.scalatest._

class LuaExprTest extends FunSuite {

    test("test bool && operation") {
        assert(LuaPrinter.print(LuaApply(LuaSelect(LuaBoolConstant(true), "$amp$amp"), List(LuaBoolConstant(false)))) == "true and false")
    }

    test("test bool || operation") {
        assert(LuaPrinter.print(LuaApply(LuaSelect(LuaBoolConstant(true), "$bar$bar"), List(LuaBoolConstant(false)))) == "true or false")
    }

    test("test bool ! operation") {
        assert(LuaPrinter.print(LuaSelect(LuaBoolConstant(true), "unary_$bang")) == "not true")
    }

    test("test double + operation") {
        assert(LuaPrinter.print(LuaApply(LuaSelect(LuaDoubleConstant(1.0), "$plus"), List(LuaDoubleConstant(2.0)))) == "1.0 + 2.0")
    }

    test("test double - operation") {
        assert(LuaPrinter.print(LuaApply(LuaSelect(LuaDoubleConstant(1.0), "$minus"), List(LuaDoubleConstant(2.0)))) == "1.0 - 2.0")
    }

    test("test double * operation") {
        assert(LuaPrinter.print(LuaApply(LuaSelect(LuaDoubleConstant(1.0), "$times"), List(LuaDoubleConstant(2.0)))) == "1.0 * 2.0")
    }

    test("test double / operation") {
        assert(LuaPrinter.print(LuaApply(LuaSelect(LuaDoubleConstant(1.0), "$div"), List(LuaDoubleConstant(2.0)))) == "1.0 / 2.0")
    }

    test("test string + operation") {
        assert(LuaPrinter.print(LuaApply(LuaSelect(LuaStringConstant("Hello "), "$plusString"), List(LuaStringConstant("world")))) == "\"Hello \" .. \"world\"")
    }

    test("calling function") {
        val testCode = LuaApply(LuaIdent("testFunc"), List(LuaApply(LuaSelect(LuaDoubleConstant(0.0), "$plus"), List(LuaDoubleConstant(1.0))), LuaApply(LuaIdent("testFunc"), List(LuaDoubleConstant(2.0), LuaDoubleConstant(3.0)))))
        assert(LuaPrinter.print(testCode) == "testFunc(0.0 + 1.0, testFunc(2.0, 3.0))")
    }

    test("define function") {
        assert(LuaPrinter.print(LuaDef("testFunc", List(LuaValDef("v0", LuaEmptyTree()), LuaValDef("v1", LuaEmptyTree())), LuaApply(LuaSelect(LuaDoubleConstant(0.0), "$plus"), List(LuaDoubleConstant(1.0))))) ==
            """function testFunc(v0, v1)
              |    return 0.0 + 1.0
              |end""".stripMargin)
    }
}