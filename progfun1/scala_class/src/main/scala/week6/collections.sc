package week6

object collections {
    val xs = Array(1, 2, 3, 44)
    xs map (x => x * 2)

    val s = "Hello World"
    s filter (c => c.isUpper)
    s exists (c => c.isUpper)
    s forall (c => c.isUpper)

    val rr: Range = 1 until 5
    val rs: Range = 1 to 5
    1 to 10 by 3
    6 to 1 by -2

    val pairs = List(1, 2, 3) zip s
    pairs.unzip

    s flatMap (c => List('.', c))

    xs.sum
    xs.max

    // list all combinations of numbers x and y where x is drawn from 1..M and y is drawn from 1..N

    val M = 4
    val N = 2
    (1 to M) flatMap (x => (1 to N) map (y => (x, y)))

    def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
        (xs zip ys).map(xy => xy._1 * xy._2).sum

    def scalarProductCase(xs: Vector[Double], ys: Vector[Double]): Double =
        (xs zip ys).map{case (x, y) => x * y}.sum

    def isPrime(n: Int): Boolean =
        if (n <= 1) false
        else
        (2 until n) forall (x => n % x != 0)

    isPrime(2)


    val n = 7
    val xss = (1 until n) map (i =>
        (1 until i) map (j => (i, j)))
    xss.flatten
    (xss foldRight Seq[(Int, Int)]())(_ ++ _)

    val yss = (1 until n) flatMap (i =>
        (1 until i) map (j => (i, j)))
    yss filter (pair => isPrime(pair._1 + pair._2))

    for {
        i <- 1 until n
        j <- 1 until i
        if isPrime(i + j)
    } yield (i, j)

    def scalarProductFor(xs: Vector[Double], ys: Vector[Double]): Double =
        (for ((x, y) <- xs zip ys) yield x * y).sum
}