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
  test("test addOne empty") {
    val l = Nil: List[Int]
    assert(List.addOne(l) === List[Int]())
  }
  test("test addOne single") {
    val l = List[Int](1)
    assert(List.addOne(l) === List[Int](2))
  }
  test("test addOne multiple") {
    val l = List[Int](1, 2, 3)
    assert(List.addOne(l) === List[Int](2, 3, 4))
  }
  test("test filter multiple") {
    val l = List[Int](1, 2, 3, 4)
    assert(List.filter(l)(x => x % 2 == 0) === List[Int](2, 4))
  }
  test("test flatMap multiple") {
    val l = List[Int](1, 2, 3)
    assert(List.flatMap(l)(i => List(i, i)) === List[Int](1, 1, 2, 2, 3, 3))
  }
  test("test filter_2 multiple") {
    val l = List[Int](1, 2, 3, 4)
    assert(List.filterViaFlatMap(l)(x => x % 2 == 0) === List[Int](2, 4))
  }
  test("test addListElements multiple") {
    val l1 = List[Int](1, 2, 3)
    val l2 = List[Int](4, 5, 6)
    assert(List.addPairWise(l1, l2) === List(5,7,9))
  }
  test("test zipWith multiple") {
    val l1 = List[Int](1, 2, 3)
    val l2 = List[Int](4, 5, 6)
    assert(List.zipWith(l1, l2)((x, y) => x + y) === List(5,7,9))
  }
}
