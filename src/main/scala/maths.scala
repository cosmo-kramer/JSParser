import fastparse.noApi._
 import WsApi._

 object maths{
 val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )
val parens: P[Int] = P( "(" ~/ addSub ~ ")" )
val factor: P[Int] = P( number | parens )

val divMul: P[Int] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
val addSub: P[Int] = P( divMul ~ (CharIn("+-").! ~/ divMul).rep ).map(eval)
val expr: P[Int]   = P( addSub )

def eval(tree: (Int, Seq[(String, Int)])) = {
  val (base, ops) = tree
  ops.foldLeft(base){ case (left, (op, right)) => op match{
    case "+" => left + right case "-" => left - right
    case "*" => left * right case "/" => left / right
  }}
}

def check(str: String, num: Int) = {
  val Parsed.Success(value, _) = expr.parse(str)
  assert(value == num)
}

def Min(a: Seq[Int]) : (Int) = {
  a.foldLeft((a(0)))
  { case ((min), e) => (math.min(min, e))}
}
def Max(a: Seq[Int]) : (Int) = {
  a.foldLeft((a(0)))
  { case ((max), e) => (math.max(max, e))}
}

val min = P(factor ~ ",").rep.map(Min)
val max = P(factor ~ ",").rep.map(Max)
check("1+1", 2)
check("1+1*2", 3)
check("(1+1*2)+(3*4*5)", 63)
check("15/3", 5)
check("63/3", 21)
check("(1+1*2)+(3*4*5)/20", 6)
check("((1+1*2)+(3*4*5))/3", 21)


/*def main(arg:Array[String]){
	println(minmax.parse("5,4,3,"))
 }
*/

}
