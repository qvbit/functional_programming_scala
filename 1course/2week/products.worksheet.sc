def prod(f: Int => Int)(a: Int, b: Int): Int = 
    if a > b then 1 else f(a) * prod(f)(a+1, b)

prod(x => x * x)(1, 5)

def fact(n : Int): Int =
    prod((x: Int) => x)(2, n)

fact(5)

// f is the map, combine is the reduce (takes two results of map and combines them to single output )
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = 
    def recur(a: Int): Int = 
        if a > b then zero
        else combine(f(a), recur(a+1))
    recur(a)


// Define sum function with above mapReduce
def sum(f: Int => Int) = 
    mapReduce(f, (x, y) => x + y, 0)

sum(fact)(1, 5)

// Now define product functionf rom above
def prodMap(f: Int => Int) =
    mapReduce(f, (x, y) => x * y, 1)

prodMap(identity)(1, 6)