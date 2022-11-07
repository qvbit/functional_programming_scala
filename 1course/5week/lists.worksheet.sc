def removeAt[T](n: Int, xs: List[T]): List[T] = xs match
    case Nil => Nil
    case y :: ys =>
        if n == 0 then ys
        else y :: removeAt(n-1, ys)


val xs = List('a', 'b', 'c', 'd')
removeAt(2, xs)


// Note that flatten can even take non-list args and in that case will just return
// that object wrapped in a list
def flatten(xs: Any): List[Any] = xs match
    case Nil => Nil
    case y :: ys => flatten(y) ::: flatten(ys)
    case _ => xs :: Nil

val ys = List(List(1, 1), 2, List(3, List(5, 8)))
flatten(ys)

extension [T](xs: List[T])
    def splitAt(n: Int) = (xs.take(n), xs.drop(n))


// Paritition vs. span
val nums = List(1, 2, 3, 4, 5, 6)
nums.partition(x => x % 2 != 0)
nums.span(x => x % 2 != 0)

def pack[T](xs: List[T]): List[List[T]] = xs match
    case Nil => Nil
    case x :: xs1 =>
        val (more, rest) = xs1.span(y => y == x)
        (x :: more) :: pack(rest)

val elems = List("a", "a", "a", "b", "c", "c", "a")
pack(elems)

def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs).map(x => (x.head, x.length))

encode(elems)
