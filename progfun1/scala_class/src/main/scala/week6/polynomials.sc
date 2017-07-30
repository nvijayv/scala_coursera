object polynomials {

    class Poly (terms0: Map[Int, Double]) {
        def this(bindings: (Int, Double)*) = this(bindings.toMap)

        val terms: Map[Int, Double] = terms0 withDefaultValue 0.0

        // inefficient than the + method below
        def plus (other: Poly) = new Poly(terms ++ (other.terms map adjustWithDefaultvalue))
        def adjust(term: (Int, Double)): (Int, Double) = {
            val (exp, coeff) = term
            terms get exp match {
                case Some(coeff2) => exp -> (coeff + coeff2)
                case None => exp -> coeff
            }
        }
        def adjustWithDefaultvalue(term: (Int, Double)): (Int, Double) = {
            val (exp, coeff) = term
            exp -> (coeff + terms(exp))
        }

        def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))
        def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
            val (exp, coeff) = term
            terms + (exp -> (coeff + terms(exp)))
        }

        override def toString: String =
            (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
    }

    val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
    val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
    p1 plus p2
    p1 + p2
    p1.terms(7)
}