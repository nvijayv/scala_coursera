// Peano numbers
abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat

    def successor: Nat = new Succ(this)

    def + (that: Nat): Nat
    def - (that: Nat): Nat
    def toString: String
}

object Zero extends Nat {
    def isZero: Boolean = true

    def predecessor: Nat = throw new IllegalStateException("predecessor of Zero")

    def + (that: Nat): Nat = that

    def - (that: Nat): Nat = {
        if (that.isZero) this
        else throw new IllegalStateException("subtracting from Zero")
    }

    override def toString: String = "0"
}

class Succ(n: Nat) extends Nat {
    def isZero: Boolean = false

    def predecessor: Nat = n

    def + (that: Nat): Nat = {
        if (that.isZero) this
        else new Succ(n + that)
    }

    def - (that: Nat): Nat = {
        if (that.isZero) this
        else n - that.predecessor
    }

    override def toString: String = {
        def count(x: Nat): Int = {
            if (x.isZero) 1
            else 1 + count(x.predecessor)
        }
        "" + count(n)
    }
}

Zero
new Succ(Zero)
Zero + Zero
Zero + new Succ(Zero)
new Succ(Zero) + Zero
new Succ(new Succ(Zero))
new Succ(new Succ(new Succ(Zero))) + new Succ(new Succ(Zero)) - new Succ(Zero)