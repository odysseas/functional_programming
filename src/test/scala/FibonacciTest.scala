import org.scalatest.funsuite.AnyFunSuite

class FibonacciTest extends AnyFunSuite {
  test("Fibonacci") {
    assert(Fibonacci.fibonacci(0) === 0)
    assert(Fibonacci.fibonacci(1) === 1)
    assert(Fibonacci.fibonacci(2) === 1)
    assert(Fibonacci.fibonacci(3) === 2)
    assert(Fibonacci.fibonacci(4) === 3)
    assert(Fibonacci.fibonacci(5) === 5)
  }
}
