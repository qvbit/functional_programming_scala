class Polynom(nonZeroTerms: Map[Int, Double]):
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    val terms = nonZeroTerms.withDefaultValue(0.0)


    // Implmentation 1 using ++
    // def + (other: Polynom): Polynom =
    //     Polynom(terms ++ other.terms.map((exp, coeff) => (exp, terms(exp) + coeff)))

    // Implementation 2 using foldLeft
    def + (other: Polynom): Polynom =
        Polynom(other.terms.foldLeft(terms)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] =
        val (exp, coeff) = term  // THis single term element is from 'other'. The original 'terms' is the zero hence is the map we're accumulating the result into
        terms + (exp -> (terms(exp) + coeff))


    override def toString =
        val termStrings =
            for (exp, coeff) <- terms.toList.sorted.reverse  // Exponents in sorted descending order
            yield
                val exponent = 
                    if exp == 0 then "" 
                    else if exp == 1 then "x"
                    else  s"x^$exp"
                s"$coeff$exponent"
        if terms.isEmpty then "0"
        else termStrings.mkString(" + ")


val x = Polynom(0 -> 2, 1 -> 3, 2 -> 1)
val z = Polynom(Map())

x + x + z
