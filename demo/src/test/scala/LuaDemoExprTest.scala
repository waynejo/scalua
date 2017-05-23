import expr.element._
import org.scalatest._
import expr._
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
}