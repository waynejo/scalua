package expr

sealed trait LuaExpr

case class LuaApply(source: LuaExpr, args: List[LuaExpr]) extends LuaExpr
case class LuaBlock(exprs: List[LuaExpr], expr: LuaExpr) extends LuaExpr
case class LuaIf(cond: LuaExpr, thenp: LuaExpr, elsep: LuaExpr) extends LuaExpr
case class LuaStaticSelect(source: LuaExpr, termName: String) extends LuaExpr
case class LuaSelect(source: LuaExpr, termName: String) extends LuaExpr
case class LuaIdent(value: String) extends LuaExpr
case class LuaDef(defName: String, params:List[LuaExpr], body:LuaExpr) extends LuaExpr
case class LuaBoolConstant(value: Boolean) extends LuaExpr
case class LuaDoubleConstant(value: Double) extends LuaExpr
case class LuaStringConstant(value: String) extends LuaExpr
case class LuaIdentConstant(ident: String) extends LuaExpr
case class LuaValDef(name: String, value: LuaExpr) extends LuaExpr
case class LuaAssign(lhs: LuaExpr, rhs: LuaExpr) extends LuaExpr
case class LuaClassDef(name: String, value: List[LuaExpr]) extends LuaExpr
case class LuaEmptyTree() extends LuaExpr