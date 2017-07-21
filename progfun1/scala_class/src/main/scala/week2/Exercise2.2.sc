object Exercise2 {

    def product(f: Int => Int)(a: Int, b: Int): Int = {
        if (a > b) 1
        else f(a) * product(f)(a+1, b)
    }

    product(x => x)(2, 4)
    product(x => x * x)(1, 6)

    def factorial(n: Int): Int = product(x => x)(1, n)

    factorial(0)
    factorial(6)

    def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unit: Int)(a: Int, b: Int): Int = {
        if (a > b) unit
        else combine(f(a), mapReduce(f, combine, unit)(a + 1, b))
    }

    def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

    factorial(7)
}