package week4

/**
  * Created by nvijayv on 23/07/17.
  */
trait List[T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]

    override def toString: String
}

class Cons[T] (val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
    override def toString: String = head.toString + " -> " + tail.toString
}

class Nil[T] extends List[T] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("Nil.head")
    def tail: Nothing = throw new NoSuchElementException("Nil.tail")

    override def toString: String = "Nil"
}

object List {
    def apply[T]: List[T] = new Nil[T]

    def apply[T](x1: T): List[T] = new Cons[T](x1, new Nil[T])

    def apply[T](x1: T, x2: T): List[T] = new Cons[T](x1, new Cons[T](x2, new Nil[T]))
}

object ExprList extends App {
    println(List())
    println(List(1))
    println(List(1, 2))
}