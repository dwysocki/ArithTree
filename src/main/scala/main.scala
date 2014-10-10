import scala.util.control.Breaks
import arithtree.tree._
import arithtree.examples.Examples

object Main {
  def main(args: Array[String]) = {
    println("Welcome to ArithTree\n")
    println("When prompted for bindings,")
    println("enter a space-separated list of pairs of the form:\n")
    println("variable number variable number ... variable number")
    /* hacky imperative grossness follows */
    for (exp <- Examples.examples) {
      val loop = new Breaks
      loop.breakable {
        while (true) {
          try {
            println("Expression = " + exp)
            print("Bindings = ")
            val bindings = getBindings
            println("Simplified = " + TreeOps.eval(exp, TreeOps.env(bindings)))
            println
            loop.break
          } catch {
            case ex: java.lang.ArrayIndexOutOfBoundsException =>
              println("Invalid number of bindings\n")
            case ex: java.lang.NumberFormatException =>
              println("Symbol must be bound to number\n")
          }
        }
      }
    }
  }
  def getBindings: Iterator[Tuple2[Var, Const]] = {
    val line: String = readLine
    val tokens: Array[String] = line.split("\\s").filterNot(_ equals "")
    if (tokens.isEmpty)
      Iterator.empty
    else
      for (p <- tokens.grouped(2))
        yield new Tuple2(Var(p(0)), Const(p(1).toInt))
  }
}
