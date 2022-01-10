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
  test("test length 1") {
    val l = List(1)
    assert(List.length(l) === 1)
  }
  test("test length 0") {
    val l = List()
    assert(List.length(l) === 0)
  }
  test("test length 3") {
    val l = List(1, 2, 3)
    assert(List.length(l) === 3)
  }
  test("test length 2") {
    val l = List(0, 0)
    assert(List.length(l) === 2)
  }
  test("reverse empty") {
    val l = List()
    val r = List.reverse(l)
    assert(r === l)
  }
  test("reverse single") {
    val l = List(1)
    val r = List.reverse(l)
    assert(r === l)
  }
  test("reverse multiple") {
    val l = List(1, 2, 3)
    val r = List.reverse(l)
    assert(r === List(3, 2, 1))
  }
}
