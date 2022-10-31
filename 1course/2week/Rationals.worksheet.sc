class Rational(x: Int, y: Int):
    def numer = x
    def denom = y

    def add(r: Rational) = 
        Rational(numer * r.denom + r.numer * denom,
                 denom * r.denom)
    
    def mul(r: Rational) = 
        Rational(numer * r.numer, denom * r.denom)

    override def toString = s"$numer/$denom"

end Rational

val x = Rational(1, 3)
val y = Rational(5, 7)
val z = Rational(3, 2)
x.add(y).mul(z)
