// Generate all pairs s.t. 1 <= j < i < n
val n = 10

def isPrime(n: Int): Boolean =
    (2 until n).forall(n % _ != 0)

val xss = (1 until n).map(i =>
    (1 until i).map(j => (i, j)))

// This gives a sequence of sequences xss. Can flatten it:

xss.flatten

// Can be condensed
(1 until n).flatMap(i =>
    (1 until i).map(j => (i, j)))

// Can find pairs s.t. x+y is prime this way:
(1 until n)
    .flatMap(i => (1 until i).map(j => (i, j)))
    .filter((x, y) => isPrime(x + y))

// Using for expression
for
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
yield(i, j)

// Scalar product
def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for (x, y) <- xs.zip(ys) yield x*y).sum


// Scala n-queens solution
def queens(n: Int) =
    def placeQueens(k: Int): Set[List[Int]] =
        if k == 0 then Set(List())
        else
            for
                queens <- placeQueens(k-1)  // Each element of queens is a solution (i.e. a vector of column numbers)
                col <- 0 until n
                if isSafe(col, queens)
            yield col :: queens
    placeQueens(n)

def isSafe(col: Int, queens: List[Int]): Boolean =
    !checks(col, 1, queens)

def checks(col: Int, delta: Int, queens: List[Int]): Boolean = queens match
    case qcol :: others =>
        qcol == col                       // Vertical check
        || (qcol - col).abs == delta      // Diagonal check. Delta here is the row diff. Want to make sure col diff and row diff are not the same else they lie on the same diagonal
        || checks(col, delta + 1, others) // Recurse and check other previous rows are also not in check
    case Nil =>
        false

queens(8)
