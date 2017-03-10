
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

  test("problem 5") {
    assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
    assert(reverse(List()) == List())
    assert(reverse(List(1)) == List(1))
    assert(reverse(List(1, 2)) == List(2, 1))
    assert(reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
  }

  test("problem 6") {
    assert(!isPalindrome(List(1, 2, 3)))
    assert(isPalindrome(List()))
    assert(isPalindrome(List(1)))
    assert(!isPalindrome(List(1, 2)))
    assert(isPalindrome(List(1, 2, 1)))
    assert(isPalindrome(List(1, 2, 2, 1)))
    assert(!isPalindrome(List(1, 2, 2, 1, 1)))
    assert(isPalindrome(List(1, 2, 3, 2, 1)))
    assert(isPalindrome(List(1, 2, 3, 3, 2, 1)))
  }

  test("problem 7") {
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
    assert(flatten(List(1, 2, List(3, List(5, List(8, 9))))) == List(1, 2, 3, 5, 8, 9))
    assert(flatten(List(1, 2, 3, 5, 8, 9)) == List(1, 2, 3, 5, 8, 9))
    assert(flatten(List(List(1, 1), 2, 3)) == List(1, 1, 2, 3))
    assert(flatten(List()) == List())
    assert(flatten(List(1)) == List(1))
    assert(flatten(List(List(1))) == List(1))
  }

  test("problem 8") {
    assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
    assert(compress(List('a, 'c, 'a, 'c, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'c, 'a, 'c, 'a, 'b, 'c, 'a, 'd, 'e))
    assert(compress(List()) == List())
    assert(compress(List('a, 'a, 'a, 'a, 'a)) == List('a))
    assert(compress(List('a, 'b, 'c, 'd, 'e)) == List('a, 'b, 'c, 'd, 'e))
  }

  test("problem 9") {
    assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
    assert(pack(List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a)) == List(List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a)))
    assert(pack(List('a, 'b, 'c, 'a, 'd, 'e)) == List(List('a), List('b), List('c), List('a), List('d), List('e)))
    assert(pack(List()) == List())
  }
}
