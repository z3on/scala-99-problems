import java.util.NoSuchElementException

object ListProblems {

  /**
    * P01 (*) Find the last element of a list.
    */
  def last[T](list: List[T]): T = list match {
    case elem :: Nil => elem
    case _ :: tail => last(tail)
    case Nil => throw new NoSuchElementException
  }

  /**
    * P02 (*) Find the last but one element of a list.
    */
  def penultimate[T](list: List[T]): T = list match {
    case elem :: _ :: Nil => elem
    case _ :: tail => penultimate(tail)
    case Nil => throw new NoSuchElementException
  }

  /**
    * P03 (*) Find the Kth element of a list.
    */
  def nth[T](index: Int, list: List[T]): T = {
    if (index < 0 || index >= list.length) throw new NoSuchElementException
    else if (index == 0) list.head
    else nth(index - 1, list.tail)
  }

  /**
    * P04 (*) Find the number of elements of a list.
    */
  def length[T](list: List[T]): Int = {
    def lengthRecursive(rest: List[T], curLength: Int): Int = rest match {
      case Nil => curLength
      case _ :: tail => lengthRecursive(tail, curLength + 1)
    }

    lengthRecursive(list, 0)
  }

  /**
    * P05 (*) Reverse a list.
    */
  def reverse[T](list: List[T]): List[T] = {
    def reverseRecursive(rest: List[T], reversed: List[T]): List[T] = rest match {
      case Nil => reversed
      case head :: tail => reverseRecursive(tail, head :: reversed)
    }

    reverseRecursive(list, Nil)
  }

  /**
    * P06 (*) Find out whether a list is a palindrome.
    */
  def isPalindrome[T](list: List[T]): Boolean = {
    def accumulateReversed(list: List[T], reversed: List[T]): Boolean =
      if (list.length == reversed.length) list == reversed
      else if (list.length == reversed.length + 1) list.tail == reversed
      else if (list.length < reversed.length) false
      else accumulateReversed(list.tail, list.head :: reversed)

    accumulateReversed(list, Nil)
  }

  /**
    * P07 (**) Flatten a nested list structure.
    */
  def flatten(list: List[Any]): List[Any] = {
    def recursiveFlatter(rest: List[Any], flattenList: List[Any]): List[Any] =
      rest match {
        // empty list
        case Nil => flattenList
        // nested list
        case (nestedHead :: nestedTail) :: tail => recursiveFlatter(tail, flattenList ++ recursiveFlatter(nestedHead :: nestedTail, Nil))
        // single element
        case head :: tail => recursiveFlatter(tail, flattenList :+ head)
      }

    recursiveFlatter(list, Nil)
  }

  /**
    * P08 (**) Eliminate consecutive duplicates of list elements.
    */
  def compress[T](list: List[T]): List[T] = {
    def recursiveCompress(rest: List[T], lastElement: Option[T]): List[T] =
      if (rest.isEmpty) Nil
      else if (lastElement.contains(rest.head)) recursiveCompress(rest.tail, lastElement)
      else rest.head :: recursiveCompress(rest.tail, Some(rest.head))

    recursiveCompress(list, Option.empty)
  }

  /**
    * P09 (**) Pack consecutive duplicates of list elements into sublists.
    * Recursive function
    */
  def packFunc[T](list: List[T]): List[List[T]] = {
    if (list.isEmpty) Nil
    else {
      val packed = packFunc(list.tail)
      if (packed.headOption.map(_.head).contains(list.head)) (list.head :: packed.head) :: packed.tail
      else List(list.head) :: packed
    }
  }

  /**
    * P09 (**) Pack consecutive duplicates of list elements into sublists.
    * Higher-order functions
    */
  def pack[T](list: List[T]): List[List[T]] = {
    list.foldRight(List.empty[List[T]]) {
      case (elem, packed) =>
        if (packed.headOption.exists(_.head == elem)) (elem :: packed.head) :: packed.tail
        else List(elem) :: packed
    }
  }

  /**
    * P10 (*) Run-length encoding of a list.
    * Recursive function
    */
  def encodeFunc[T](list: List[T]): List[(Int, T)] = {
    def encodeRecursive(grouped: List[List[T]]): List[(Int, T)] =
      if (grouped.isEmpty) Nil
      else (grouped.head.length, grouped.head.head) :: encodeRecursive(grouped.tail)

    encodeRecursive(pack(list))
  }

  /**
    * P10 (*) Run-length encoding of a list.
    * Higher-order functions
    */
  def encode[T](list: List[T]): List[(Int, T)] = {
    pack(list).map(nestedList => (nestedList.length, nestedList.head))
  }

  /**
    * P11 (*) Modified run-length encoding.
    * Recursive function
    */
  def encodeModifiedFunc[T](list: List[T]): List[Any] = {
    def encodeModifiedRecursive(stats: List[(Int, T)]): List[Any] =
      if (stats.isEmpty) Nil
      else if (stats.head._1 == 1) stats.head._2 :: encodeModifiedRecursive(stats.tail)
      else stats.head :: encodeModifiedRecursive(stats.tail)

    encodeModifiedRecursive(encode(list))
  }

  /**
    * P11 (*) Modified run-length encoding.
    * Higher-order functions
    */
  def encodeModified[T](list: List[T]): List[Any] = {
    encode(list).map {
      case (count, elem) => if (count == 1) elem else (count, elem)
    }
  }

  /**
    * P12 (**) Decode a run-length encoded list.
    * Recursive function
    */
  def decodeFunc[T](list: List[(Int, T)]): List[T] = list match {
    case Nil => Nil
    case (1, el) :: tail => el :: decodeFunc(tail)
    case (count, el) :: tail => el :: decodeFunc((count - 1, el) :: tail)
  }

  /**
    * P12 (**) Decode a run-length encoded list.
    * Higher-order functions
    */
  def decode[T](list: List[(Int, T)]): List[T] = list.flatMap {
    case (count, el) => List.fill(count)(el)
  }


  /**
    * P13 (**) Run-length encoding of a list (direct solution).
    * Recursive function
    */
  def encodeDirectFunc[T](list: List[T]): List[(Int, T)] = {
    if (list.isEmpty) Nil
    else {
      val encoded = encodeDirectFunc(list.tail)
      if (encoded.headOption.forall(tuple => tuple._2 != list.head)) (1, list.head) :: encoded
      else (encoded.head._1 + 1, encoded.head._2) :: encoded.tail
    }
  }

  /**
    * P13 (**) Run-length encoding of a list (direct solution).
    * Higher-order functions
    */
  def encodeDirect[T](list: List[T]): List[(Int, T)] = {
    list.foldRight(List.empty[(Int, T)]) {
      case (elem, encoded) =>
        if (encoded.headOption.exists(_._2 == elem)) (encoded.head._1 + 1, elem) :: encoded.tail
        else (1, elem) :: encoded
    }
  }

  /**
    * P14 (*) Duplicate the elements of a list.
    * Recursive function
    */
  def duplicateFunc[T](list: List[T]): List[T] =
    if (list.isEmpty) Nil
    else list.head :: list.head :: duplicate(list.tail)

  /**
    * P14 (*) Duplicate the elements of a list.
    * Higher-order functions
    */
  def duplicate[T](list: List[T]): List[T] =
    list.flatMap(el => List(el, el))

  /**
    * P15 (**) Duplicate the elements of a list a given number of times.
    * Recursive function
    */
  def duplicateNFunc[T](n: Int, list: List[T]): List[T] = {
    def nTimesList(rest: List[T], currentN: Int): List[T] =
      if (rest.isEmpty) Nil
      else if (currentN == 0) nTimesList(rest.tail, n)
      else rest.head :: nTimesList(rest, currentN - 1)

    nTimesList(list, n)
  }

  /**
    * P15 (**) Duplicate the elements of a list a given number of times.
    * Higher-order functions
    */
  def duplicateN[T](n: Int, list: List[T]): List[T] =
    list.flatMap(List.fill(n)(_))

  /**
    * P16 (**) Drop every Nth element from a list.
    * Recursive function
    */
  def dropFunc[T](n: Int, list: List[T]): List[T] = {
    def nTimesList(rest: List[T], currentN: Int): List[T] =
      if (rest.isEmpty) Nil
      else if (currentN == 1) nTimesList(rest.tail, n)
      else rest.head :: nTimesList(rest.tail, currentN - 1)

    nTimesList(list, n)
  }

  /**
    * P16 (**) Drop every Nth element from a list.
    * Higher-order functions
    */
  def drop[T](n: Int, list: List[T]): List[T] =
    list.zipWithIndex
      .filter {
        case (_, index) => ((index + 1) % n) > 0
      }
      .map { case (el, _) => el }

  /**
    * P17 (*) Split a list into two parts.
    * Recursive function
    */
  def splitFunc[T](n: Int, list: List[T]): (List[T], List[T]) = {
    def nTimesList(rest: List[T], currentN: Int): (List[T], List[T]) =
      if (rest.isEmpty) (Nil, Nil)
      else nTimesList(rest.tail, currentN + 1) match {
        case (before, after) =>
          if (currentN < n) (rest.head :: before, after)
          else (before, rest.head :: after)
      }

    nTimesList(list, 0)
  }

  /**
    * P17 (*) Split a list into two parts.
    * Higher-order functions
    */
  def split[T](n: Int, list: List[T]): (List[T], List[T]) =
  // Obvious: list.splitAt(n)
    (list.take(n), list.drop(n))

  /**
    * P18 (**) Extract a slice from a list.
    * Recursive function
    */
  def sliceFunc[T](from: Int, to: Int, list: List[T]): List[T] = {
    if (to == 0 || list.isEmpty) Nil
    else if (from == 0) list.head :: sliceFunc(from, to - 1, list.tail)
    else sliceFunc(from - 1, to - 1, list.tail)
  }

  /**
    * P18 (**) Extract a slice from a list.
    * Higher-order functions
    */
  def slice[T](from: Int, to: Int, list: List[T]): List[T] =
  // Obvious: list.slice(from, to)
    list.drop(from).take(to - from)

  /**
    * P19 (**) Rotate a list N places to the left.
    * Higher-order functions
    */
  def rotate[T](n: Int, list: List[T]): List[T] =
    if (n < 0) list.takeRight(-n) ++ list.dropRight(-n)
    else list.drop(n) ++ list.take(n)

  /**
    * P20 (*) Remove the Kth element from a list.
    * Recursive function
    */
  def removeAtFunc[T](n: Int, list: List[T]): (List[T], T) = {
    if (list.isEmpty || n < 0) throw new IndexOutOfBoundsException
    else if (n == 0) (list.tail, list.head)
    else removeAtFunc(n - 1, list.tail) match {
      case (rest, removed) => (list.head :: rest, removed)
    }
  }

  /**
    * P20 (*) Remove the Kth element from a list.
    * Recursive function
    */
  def removeAt[T](n: Int, list: List[T]): (List[T], T) = {
    if (n < 0) throw new IndexOutOfBoundsException
    else list.splitAt(n) match {
      case (_, Nil) => throw new NoSuchElementException
      case (left, right) => (left ++ right.tail, right.head)
    }
  }

  /**
    * P21 (*) Insert an element at a given position into a list.
    * Recursive function
    */
  def insertAtFunc[T](newElement: T, n: Int, list: List[T]): List[T] = {
    if (n == 0) newElement :: list
    else if (list.isEmpty || n < 0) throw new IndexOutOfBoundsException
    else list.head :: insertAtFunc(newElement, n - 1, list.tail)
  }

  /**
    * P21 (*) Insert an element at a given position into a list.
    * Recursive function
    */
  def insertAt[T](newElement: T, n: Int, list: List[T]): List[T] = {
    if (n < 0) throw new IndexOutOfBoundsException
    else list.splitAt(n) match {
      case (left, right) =>
        if (left.length < n) throw new IndexOutOfBoundsException
        else left ++ (newElement :: right)
    }
  }

}
