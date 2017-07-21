object exercise {
    "Hello World"

    def abs(x: Double): Double = if (x >= 0.0) x else -x

    def sqrt(x: Double): Double = {
        def sqrtIter(guess: Double): Double =
            if (isGoodEnough(guess)) guess
            else sqrtIter(improve(guess))

        def isGoodEnough(guess: Double): Boolean =
            abs(guess * guess - x) / x < 0.0001

        def improve(guess: Double): Double =
            (guess + x / guess) / 2

        sqrtIter(1.0)
    }

    sqrt(2)
    sqrt(1e-6)
    sqrt(1e60)

    def factorial(n: Int): Int = {
        def loop(acc: Int, n: Int): Int = {
            if (n == 0) acc
            else loop(acc * n, n - 1)
        }

        loop(1, n)
    }

    factorial(4)
}