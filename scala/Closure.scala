object Closure {
  def main(args: Array[String]) {
    println("mul(1) = " + mul(1))
    println("mul(2) = " + mul(2))
  } 
  var a = 2
  val mul = (i:Int) => i * a // This is the closure
}
