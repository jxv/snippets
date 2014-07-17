
abstract class Tree
case class Node(left: Tree, right: Tree) extends Tree
case class Leaf(value: String) extends Tree

def show(tree: Tree): String = tree match {
  case Node(left, right) => "(" + show(left) + "," + show(right) + ")"
  case Leaf(value) => value
}

object PatternMatch {
  def main(args: Array[String]) {
    var x: Tree = Leaf("a")
    var y: Tree = Leaf("b")
    var z: Tree = Leaf("c")
    var n: Tree = Node(Node(x,y),z)
    println(show(n))
  }
}

PatternMatch.main(Array())

