package fpinscala.datastructures

class ListTest extends org.scalatest.funsuite.AnyFunSuite {
  test("test setHead") {
    val l = List(1, 2)
    assert(List.setHead(l, 3) === List(3, 2))
  }
  test("test drop") {
    val l = List(1, 2, 3, 4)
    assert(List.drop(l, 2) === List(3, 4))
    assert(List.drop(l, 5) === Nil)
  }
}
