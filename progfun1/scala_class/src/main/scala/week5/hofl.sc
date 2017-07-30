// Higher Order Functions over Lists

object hofl {

    def squareList(xs: List[Int]): List[Int] =
        xs match {
            case Nil => xs
            case y :: ys => (y * y) :: squareList(ys)
        }

    def squareListUsingMap(xs: List[Int]): List[Int] =
        xs map (x => x * x)

    def pack[T](xs: List[T]): List[List[T]] =
        xs match {
            case Nil => Nil
            case x :: xs1 =>
                val (xCopies, rest) = xs span (y => y == x)
                xCopies :: pack(rest)
        }

    def encode[T](xs: List[T]): List[(T, Int)] =
        pack(xs) map (ys => (ys.head, ys.length))

    def sumReduceLeft(xs: List[Int]): Int = (0 :: xs) reduceLeft (_ + _)
    def productReduceLeft(xs: List[Int]): Int = (1 :: xs) reduceLeft (_ * _)

    def sumFoldLeft(xs: List[Int]): Int = (xs foldLeft 0) (_ + _)
    def productFoldLeft(xs: List[Int]): Int = (xs foldRight 1) (_ * _)

    def concat[T](xs: List[T], ys: List[T]): List[T] =
        (xs foldRight ys) (_ :: _)

    val nums = List(2, -4, 5, 7, 1)
    val fruits = List("apple", "pineapple", "banana", "sapota", "pomegrenate")
    val data = List("a", "a", "a", "b", "c", "c", "a")

    nums filter (x => x > 0)
    nums filterNot (x => x > 0)
    nums partition (x => x > 0)

    nums takeWhile (x => x > 0)
    nums dropWhile (x => x > 0)
    nums span (x => x > 0)

    pack(data)
    encode(data)

    sumReduceLeft(nums)
    sumFoldLeft(nums)

    productReduceLeft(nums)
    productFoldLeft(nums)

    concat(nums, fruits)
}