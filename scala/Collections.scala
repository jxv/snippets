object Collections {
  val mylist = List(1,2,3,4)
  var myset = Set(1,3,5,7)
  val mymap = Map("a" -> 0, "b" -> 1, "abcd" -> 3)
  val mytuple = (0, "snd elem")
  val myopt : Option[Int] = Some(100)
  
  def main(args: Array[String]) {
    println(mylist)
    println(myset)
    println(mymap)
    println(mytuple)
    println(myopt)
  }
}
