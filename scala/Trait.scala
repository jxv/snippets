
/* Traits are essentially Java Interfaces, not Haskell type-classes :(  */
trait Num {
  def add(x: Int): Unit = sub(-x)
  def sub(x: Int): Unit = add(-x)
  def negate(): Unit
}

class MyInt(vc: Int) extends Num {
  var v: Int = vc

  override def add(x: Int) = v += x
  def negate() = v = -v
}

object Trait {
  def main(args: Array[String]) {
    val n = new MyInt(100)
    n.sub(9)
    n.negate()
    
    println(n.v)
  }
}

Trait.main(Array())
