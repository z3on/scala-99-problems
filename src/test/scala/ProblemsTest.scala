
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

  test("problem 9 - recursive function") {
    assert(packFunc(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
    assert(packFunc(List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a)) == List(List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a)))
    assert(packFunc(List('a, 'b, 'c, 'a, 'd, 'e)) == List(List('a), List('b), List('c), List('a), List('d), List('e)))
    assert(packFunc(List()) == List())
  }

  test("problem 9 - higher-order functions") {
    assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
    assert(pack(List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a)) == List(List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a)))
    assert(pack(List('a, 'b, 'c, 'a, 'd, 'e)) == List(List('a), List('b), List('c), List('a), List('d), List('e)))
    assert(pack(List()) == List())
  }

  test("problem 10 - recursive function") {
    assert(encodeFunc(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    assert(encodeFunc(List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a)) == List((9, 'a)))
    assert(encodeFunc(List('a, 'b, 'c, 'a, 'd, 'e)) == List((1, 'a), (1, 'b), (1, 'c), (1, 'a), (1, 'd), (1, 'e)))
    assert(encodeFunc(List()) == List())
  }

  test("problem 10 - higher-order functions") {
    assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    assert(encode(List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a)) == List((9, 'a)))
    assert(encode(List('a, 'b, 'c, 'a, 'd, 'e)) == List((1, 'a), (1, 'b), (1, 'c), (1, 'a), (1, 'd), (1, 'e)))
    assert(encode(List()) == List())
  }

  test("problem 11 - recursive function") {
    assert(encodeModifiedFunc(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
    assert(encodeModifiedFunc(List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a)) == List((9, 'a)))
    assert(encodeModifiedFunc(List('a, 'b, 'c, 'a, 'd, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
    assert(encodeModifiedFunc(List()) == List())
  }

  test("problem 11 - higher-order functions") {
    assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
    assert(encodeModified(List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a)) == List((9, 'a)))
    assert(encodeModified(List('a, 'b, 'c, 'a, 'd, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
    assert(encodeModified(List()) == List())
  }

  test("problem 12 - recursive function") {
    assert(decodeFunc(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(decodeFunc(List((9, 'a))) == List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a))
    assert(decodeFunc(List((1, 'a), (1, 'b), (1, 'c), (1, 'a), (1, 'd), (1, 'e))) == List('a, 'b, 'c, 'a, 'd, 'e))
    assert(decodeFunc(List()) == List())
  }

  test("problem 12 - higher-order functions") {
    assert(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(decode(List((9, 'a))) == List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a))
    assert(decode(List((1, 'a), (1, 'b), (1, 'c), (1, 'a), (1, 'd), (1, 'e))) == List('a, 'b, 'c, 'a, 'd, 'e))
    assert(decode(List()) == List())
  }

  test("problem 13 - recursive function") {
    assert(encodeDirectFunc(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    assert(encodeDirectFunc(List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a)) == List((9, 'a)))
    assert(encodeDirectFunc(List('a, 'b, 'c, 'a, 'd, 'e)) == List((1, 'a), (1, 'b), (1, 'c), (1, 'a), (1, 'd), (1, 'e)))
    assert(encodeDirectFunc(List()) == List())
  }

  test("problem 13 - higher-order functions") {
    assert(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    assert(encodeDirect(List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a)) == List((9, 'a)))
    assert(encodeDirect(List('a, 'b, 'c, 'a, 'd, 'e)) == List((1, 'a), (1, 'b), (1, 'c), (1, 'a), (1, 'd), (1, 'e)))
    assert(encodeDirect(List()) == List())
  }

  test("problem 14 - recursive function") {
    assert(duplicateFunc(List('a, 'b, 'c, 'd, 'e)) == List('a, 'a, 'b, 'b, 'c, 'c, 'd, 'd, 'e, 'e))
    assert(duplicateFunc(List('a, 'a, 'a, 'a)) == List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a))
    assert(duplicateFunc(List('a, 'b, 'a)) == List('a, 'a, 'b, 'b, 'a, 'a))
    assert(duplicateFunc(List()) == List())
  }

  test("problem 14 - higher-order functions") {
    assert(duplicate(List('a, 'b, 'c, 'd, 'e)) == List('a, 'a, 'b, 'b, 'c, 'c, 'd, 'd, 'e, 'e))
    assert(duplicate(List('a, 'a, 'a, 'a)) == List('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a))
    assert(duplicate(List('a, 'b, 'a)) == List('a, 'a, 'b, 'b, 'a, 'a))
    assert(duplicate(List()) == List())
  }
}
