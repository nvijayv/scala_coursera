import math.Ordering

object mergesort {
    def last[T](xs: List[T]): T = xs match {
        case List() => throw new Error("last of empty list")
        case List(x) => x
        case y :: ys => last(ys)
    }

    def init[T](xs: List[T]): List[T] = xs match {
        case List() => throw new Error("init of empty list")
        case List(x) => List()
        case y :: ys => y :: init(ys)
    }

    def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
        case List() => ys
        case z :: zs => z :: concat(zs, ys)
    }

    def reverse[T](xs: List[T]): List[T] = xs match {
        case List() => xs
        case y :: ys => reverse(ys) ++ List(y)
    }

    def removeAt[T](xs: List[T], n: Int): List[T] = (xs take n) ::: (xs drop n+1)

    /*def mergeNested(xs: List[Int], ys: List[Int]): List[Int] = xs match {
        case Nil => ys
        case x :: xs1 =>
            ys match {
                case Nil => xs
                case y :: ys1 =>
                    if (x < y) x :: mergeNested(xs1, ys)
                    else y :: mergeNested(xs, ys1)
            }
    }*/

    def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
        def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x :: xs1, y :: ys1) =>
                if (lt(x, y)) x :: merge(xs1, ys)
                else y :: merge(xs, ys1)
        }

        val n = xs.length / 2
        if (n == 0) xs
        else {
            val (fst, snd) = xs.splitAt(n)
            merge(msort(fst)(lt), msort(snd)(lt))
        }
    }

    def msortOrdering[T](xs: List[T])(ord: Ordering[T]): List[T] = {
        def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x :: xs1, y :: ys1) =>
                if (ord.lt(x, y)) x :: merge(xs1, ys)
                else y :: merge(xs, ys1)
        }

        val n = xs.length / 2
        if (n == 0) xs
        else {
            val (fst, snd) = xs.splitAt(n)
            merge(msortOrdering(fst)(ord), msortOrdering(snd)(ord))
        }
    }

    def msortImplicitOrdering[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
        def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x :: xs1, y :: ys1) =>
                if (ord.lt(x, y)) x :: merge(xs1, ys)
                else y :: merge(xs, ys1)
        }

        val n = xs.length / 2
        if (n == 0) xs
        else {
            val (fst, snd) = xs.splitAt(n)
            merge(msortImplicitOrdering(fst), msortImplicitOrdering(snd))
        }
    }

    val nums = List(2, -4, 5, 7, 1)
    val fruits = List("apple", "pineapple", "banana", "sapota", "pomegrenate")

    msort(nums)((x: Int, y: Int) => x < y)
    msortOrdering(nums)(Ordering.Int)
    msortImplicitOrdering(nums)

    msort(fruits)((x: String, y: String) => x.compareTo(y) < 0)
    msortOrdering(fruits)(Ordering.String)
    msortImplicitOrdering(fruits)
}