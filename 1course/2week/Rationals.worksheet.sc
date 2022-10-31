class Rational(x: Int, y: Int):
    require(y > 0, s"Denom must be positive, was $x/$y")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int =
        if b == 0 then a else gcd(b, a % b)
    
    val numer = x / gcd(x.abs, y)
    val denom = y / gcd(x.abs, y)

    def add(r: Rational) = 
        Rational(numer * r.denom + r.numer * denom,
                 denom * r.denom)
    
    def mul(r: Rational) = 
        Rational(numer * r.numer, denom * r.denom)

    def neg = Rational(-numer, denom)

    def sub(r: Rational) = add(r.neg)

    def less(that: Rational): Boolean =
        numer * that.denom < that.numer * denom
    
    def max(that: Rational): Rational =
        if this.less(that) then that else this

    override def toString = s"$numer/$denom"
end RationalE

val x = Rational(1, 3)
val y = Rational(5, 7)
val z = Rational(3, 2)
x.add(y).mul(z)
x.sub(y).sub(z)
