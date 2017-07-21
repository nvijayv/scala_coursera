package funsets

object Main extends App {
    import FunSets._
    println(singletonSet(1))
    println(contains(singletonSet(1), 1))
    println(contains(union(singletonSet(1), singletonSet(2)), 3))
    println(union(singletonSet(1), singletonSet(2)))
    println(intersect(union(singletonSet(1), singletonSet(2)), union(singletonSet(1), singletonSet(3))))

    def isEven: Int => Boolean = x => x % 2 == 0

    printSet(filter(isEven, isEven))

    printSet(map(isEven, x => x * 3))

    printSet(map(union(singletonSet(1), singletonSet(2)), x => x - 100))

    val evensUpto10: Set = x => (x % 2 == 0) && math.abs(x) <= 10
    printSet(map(evensUpto10, x => x - 1))
}
