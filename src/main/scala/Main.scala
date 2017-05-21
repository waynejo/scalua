
import expr._
import expr.element.{bool, double}

object Main {

    def main(args: Array[String]): Unit = {
        val code = LuaPrinter.print(double(1.0) + double(2.0) * double(3.0))
//        println(code)
        val boolExpr = !(bool(true) && bool(false))
        val test = 'value

        val assign = SDExprAssign(SDExprVariable("bar"), boolExpr)
        val assis2 = SDExprAssign(SDExprVariable("foo"), boolExpr)

        println(LuaPrinter.asString(SDExprIF(boolExpr, assign, assis2)))
        val testFunc = SDFunction0("testFunc", assign)
        println(LuaPrinter.asString(testFunc()))
        println(LuaPrinter.asString(SDFunction1[bool]("testFunc2", SDExprReturn(boolExpr))))
    }
}
