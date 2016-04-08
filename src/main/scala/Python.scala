val word = regex("[^\\s]+".r)
val WS = regex("\s+".r)
val maybeWS = regex("\s*")
val indent = slice(maybeWS).map(_.length)
val retType = arrow *> type <* (":" +> ("\n" | comment))
val type = word
val argPair = (arg <* ":" +> type) map ( (_._1._1, _._2) )
val args = manyWithSep(",")(argPair)
val funcArgs = surroundWS("(",")")(args)

def surroundWS[A,B,C](s1: Parser[A], s2: Parser[B])(p: Parser[C]): Parser[C] =
  //(s1 +> p +> s2) map (_._2._1)
  (s1 <* p *> s2)
val frontMatter =  (indent *> "def" *> WS)
val defLine =  frontMatter *> funcName +> funcArgs +> retType 

case class FuncType(indent: Int, fn: String, argTypes: List[(Arg,Type)], ret: Type) {

def py2Format: String = { 
  val indentStr = " " * indent
  val argNames = argTypes map ( _._1 ) mkString ", "
  val types = argTypes map ( _._2 ) mkString ", "
  //val argTypes = args map ( (_._1 + ", :" + _._2) ) mkString "\n" 
  f"""${indentStr}def $fn ($argNames): # type: ($types) -> $ret"""
}
def py3Format: String = {
  val formattedArgs = args map ( (_._1 + ": " + _._2) ) mkString ", " 
  f"""${indentStr}def $fn ($formattedArgs) -> $ret:""" 
}
// filter on whether or not I was able to parse it 
// should be able to test py3 parsing with FuncType obj -> py3Format -> Functype obj equality
