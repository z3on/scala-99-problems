
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

}
