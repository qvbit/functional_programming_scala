abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

def weight(tree: CodeTree): Int = tree match
  case Fork(left, right, chars, weight) => weight
  case Leaf(char, weight) => weight

def chars(tree: CodeTree): List[Char] = tree match
  case Fork(left, right, chars, weight) => chars
  case Leaf(char, weight) => List(char)

def makeCodeTree(left: CodeTree, right: CodeTree) =
  Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

def string2Chars(str: String): List[Char] = str.toList


def times(chars: List[Char]): List[(Char, Int)] = chars match
  case List() => Nil
  case x :: xs => (x, xs.count(c => c == x) + 1) :: times(xs.filter(c => c != x))

def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
  def asLeaf(pair: (Char, Int)): Leaf = Leaf(pair._1, pair._2)

  def insert(x: Leaf, xs: List[Leaf]): List[Leaf] = xs match
    case List() => List(x)
    case y :: ys => if x.weight < y.weight then x :: xs else y :: insert(x, ys)
  
  def isort(xs: List[(Char, Int)]): List[Leaf] = xs match
    case List() => List()
    case y :: ys => insert(asLeaf(y), isort(ys))

  isort(freqs)

def singleton(trees: List[CodeTree]): Boolean = trees match
  case x :: Nil => true
  case _ => false

def combine(trees: List[CodeTree]): List[CodeTree] = trees match
  case List() => trees
  case x :: Nil => trees
  case x1 :: x2 :: xs => insert(makeCodeTree(x1, x2), xs)

  def insert(x: CodeTree, xs: List[CodeTree]): List[CodeTree] = xs match
    case List() => List(x)
    case y :: ys => if weight(x) < weight(y) then x :: xs else y :: insert(x, ys)

val l = List('a', 'b', 'a', 'c', 'c', 'f', 'c', 'f', 'a', 'f', 'f', 'f', 'f')

val l2 = times(l)

val l3 = makeOrderedLeafList(l2)

l3

combine(l3)





