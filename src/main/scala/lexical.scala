object WsApi extends fastparse.WhitespaceApi.Wrapper({
  import fastparse.all._
  (" " | "\n").rep;
})

import fastparse.noApi._
import WsApi._

object lexical {
  //all js lexical
  //val space = Set(" ")
  val symbol = P(arithOperator | asignOperator | stringOperator | compOperator | binOperator | logiOperator)
  val arithOperator = P(CharIn("+", "-", "*", "/", "%")).!
  val incredecre = P(StringIn("++", "--"))
  val asignOperator = P(StringIn("=", "+=", "-=", "*=", "/=", "%="))
  val stringOperator = P(StringIn("+", "+="))
  val compOperator = P(StringIn("==", "===", "!=", ">", "<", ">=", "<="))
  val logiOperator = P(StringIn("&&", "||", "!"))
  val binOperator = P(CharIn("&", "|", "~", "^", "<<", ">>"))
  val boolean = P("True" | "False")
  val brackets = P(CharIn("(", "{", "[", ")", "}", "]"))
  //no -->deci,octa,binary,hex,exp,float,pointfloat,fraction,long,signed,unsigned
  val digit = P(CharIn('0' to '9')).!
  val number = P(digit | "." | "e" | "-").rep.!
  val arr_char = (!(brackets | ",") ~ AnyChar)
  val narray = P("[" ~ (number ~ ",").rep.? ~ number ~ "]").!
  val sarray = P("[" ~ string ~ ("," ~ string).rep.? ~ "]").!
  val array = P(narray | sarray)
  val json1 = P("{" ~ string ~ ":" ~ string ~ ("," ~ string ~ ":" ~ string).rep.? ~ "}").!
  val json = P("{" ~ string ~ ":" ~ "[" ~/ json1 ~ ("," ~ json1).rep.? ~ "]" ~ "}").!

  def keyword(s: String) = s ~ !(symbol | digit | "_")

  //keywork can be symbol digit or _
  //val kw = P("key"~ digit).!.map{case s =>keyword(s)}
  val lcLetter = P(CharIn('a' to 'z')).!
  val ucLetter = P(CharIn('A' to 'Z')).!
  val letter = P(lcLetter | ucLetter)
  //js uses unicode not yet added here
  val identifier = P("$" | "_" | "\\" | letter | digit).rep.!.filter(!keywordList.contains(_))
  val string_token_char = ((!("\"" | "'") ~ AnyChar))
  val string = P(("\"" ~/ string_token_char.rep.! ~/ "\"") | ("\'" ~/ string_token_char.rep.! ~/ "\'"))
  val variable = P("var" ~ identifier).!
  val sComment = P("//" ~ CharsWhile(_ != ('\n'), min = 0)).!
  val lComment = P("/*" ~ CharsWhile(_ != ('*' & '/'), min = 0) ~ "*/".?).!
  val keywordList = Set(
    "abstract", "arguments", "boolean", "break", "byte",
    "case", "catch", "char", "class*", "const",
    "continue", "debugger", "default", "delete", "do",
    "double", "else", "enum*", "eval", "export*",
    "extends*", "false", "final", "finally", "float",
    "for", "function", "goto", "if", "implements",
    "import*", "in", "instanceof", "int", "interface",
    "let", "long", "native", "new", "null",
    "package", "private", "protected", "public", "return",
    "short", "static", "super*", "switch", "synchronized",
    "this", "throw", "throws", "transient", "true",
    "try", "typeof", "var", "void", "volatile",
    "while", "with", "yield")
  val escapeSeq = P("\\" ~ CharsWhile(_ != ' ', min = 0)).!
  val exp = P(identifier ~ (symbol ~ identifier).rep.?)

}
