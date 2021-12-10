import MyModule.isSorted
import org.scalatest.funsuite.AnyFunSuite

class IsSortedTest extends AnyFunSuite{
  test("Test isSorted") {
    assert(isSorted(Array(0), (x: Int, y: Int) => x <= y))
    assert(isSorted(Array(1), (x: Int, y: Int) => x <= y))
    assert(isSorted(Array(5), (x: Int, y: Int) => x <= y))
    assert(isSorted(Array(0, 1), (x: Int, y: Int) => x <= y))
    assert(isSorted(Array(0, 0), (x: Int, y: Int) => x <= y))
    assert(isSorted(Array(1, 1), (x: Int, y: Int) => x <= y))
    assert(isSorted(Array(1, 1, 1), (x: Int, y: Int) => x <= y))
    assert(isSorted(Array(1, 2, 3), (x: Int, y: Int) => x <= y))
    assert(!isSorted(Array(1, 0), (x: Int, y: Int) => x <= y))
    assert(!isSorted(Array(1, 1, 0), (x: Int, y: Int) => x <= y))
    assert(!isSorted(Array(3, 2, 1), (x: Int, y: Int) => x <= y))
    assert(!isSorted(Array(3, 1, 2), (x: Int, y: Int) => x <= y))
    assert(!isSorted(Array(1, 3, 2), (x: Int, y: Int) => x <= y))
  }
}
