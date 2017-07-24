package week4.variance

/**
  * Created by nvijayv on 24/07/17.
  */
trait List[+T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
    def prepend[U >: T](elem: U) = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) {
    def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("Nil.head")
    def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object test {
    val x: List[String] = Nil
    x.prepend(x)
}