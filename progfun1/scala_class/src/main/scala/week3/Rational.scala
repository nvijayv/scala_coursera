package week3

/**
  * Created by nvijayv on 20/07/17.
  */
class Rational(x: Int, y: Int) {

    require(y != 0, "denominator must be non-zero")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int = {
        if (b == 0) a else gcd(b, a % b)
    }
    private val g = gcd(x, y)

    val numer: Int = x / g
    val denom: Int = y / g

    def + (that: Rational): Rational = {
        new Rational(
            numer * that.denom + that.numer * denom,
            denom * that.denom)
    }

    def unary_- : Rational = new Rational(-numer, denom)

    def - (that: Rational): Rational = {
        this + -that
    }

    def < (that: Rational): Boolean = {
        numer * that.denom < that.numer * denom
    }

    def max(that: Rational): Rational = {
        if (this < that) that else this
    }

    override def toString: String = numer + "/" + denom
}
