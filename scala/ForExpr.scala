
// expr #1 and #2 are eqv.

object ForExpr {
  def main(args: Array[String]) {
    var N: Int = 10

    // expr #1
    val ls = for {
      x <- 2 to N
      y <- 2 to x
      if (x % y == 0)
    } yield (x, y)
    println(ls)

    // expr #2
    val ls_ = 
      (2 to N) flatMap (x =>
        (2 to x) withFilter (y =>
          x % y == 0) map (y => (x, y)))
    println(ls_)
  }
}

