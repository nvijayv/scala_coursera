import math.abs

object Exercise3 {

    val tolerance = 1.0e-4

    def isCloseEnough(x: Double, y: Double): Boolean = {
        abs((x - y)/x) < tolerance
    }

    def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
        def iterate(guess: Double): Double = {
            println("guess = " + guess)
            val next = f(guess)
            if (isCloseEnough(next, guess)) next
            else iterate(next)
        }
        iterate(firstGuess)
    }

    fixedPoint(x => 1 + x/2)(1)

    // def sqrt(x: Double): Double = fixedPoint(y => x/y)(1.0) doesn't converge for sqrt(2.0) but oscillates between 1 & 2.
    // To help converge we take average of arg and f(arg) below

    def sqrt(x: Double): Double = fixedPoint(y => (y + x/y) / 2)(1.0)

    sqrt(2.0)

    def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

    def newSqrt(x: Double): Double = fixedPoint(averageDamp(y => x/y))(1.0)

    newSqrt(2.0)
}