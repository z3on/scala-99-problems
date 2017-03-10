
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
    case elem :: last :: Nil => elem
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
    */
  def pack[T](list: List[T]): List[List[T]] = {
    if (list.isEmpty) Nil
    else {
      val packed = pack(list.tail)
      if (packed.headOption.map(_.head).contains(list.head)) (list.head :: packed.head) :: packed.tail
      else List(list.head) :: packed
    }
  }

}
