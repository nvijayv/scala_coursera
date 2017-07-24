package week4

/**
  * Created by nvijayv on 24/07/17.
  */
trait Expr {
    def eval(e: Expr): Int = e match {
        case Number(n) => n
        case Sum(e1, e2) => eval(e1) + eval(e2)
        case Prod(e1, e2) => eval(e1) * eval(e2)
    }
    def show(e: Expr): String = e match {
        case Number(n) => n.toString
        case Sum(e1, e2) => "(" + show(e1) + " + " + show(e2) + ")"
        case Prod(e1, e2) => "(" + show(e1) + " * " + show(e2) + ")"
        case Var(name) => name
    }
}

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(name: String) extends Expr

object Main extends App {
    val e: Expr = Sum(Number(1), Number(44))
    println(e.show(e))
    val f: Expr = Prod(Sum(Var("x"), Number(2)), Number(3))
    println(f.show(f))
}
