
import java.util.NoSuchElementException

import ListProblems._
import org.scalatest.FunSuite

class ProblemsTest extends FunSuite {

  test("problem 1") {
    assert(last(List(1, 2, 3)) == 3)
    assert(last(List(1)) == 1)
    assertThrows[NoSuchElementException](last(List()))
  }

  test("problem 2") {
    assert(penultimate(List(1, 2, 3)) == 2)
    assert(penultimate(List(6, 7, 8, 9, 0)) == 9)
    assertThrows[NoSuchElementException](penultimate(List()))
    assertThrows[NoSuchElementException](penultimate(List(1)))
  }

  test("problem 3") {
    assert(nth(2, List(1, 2, 3)) == 3)
    assert(nth(1, List(1, 2, 3)) == 2)
    assert(nth(0, List(1, 2, 3)) == 1)
    assertThrows[NoSuchElementException](nth(-1, List(0, 1, 2)))
    assertThrows[NoSuchElementException](nth(4, List(0, 1, 2)))
    assertThrows[NoSuchElementException](nth(0, List()))
  }

  test("problem 4") {
    assert(length(List(1, 2, 3)) == 3)
    assert(length(List()) == 0)
    assert(length(List(1, 2, 3, 5, 4)) == 5)
  }
}
