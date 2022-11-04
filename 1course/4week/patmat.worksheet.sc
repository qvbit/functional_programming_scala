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


  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = 
    def decodeHelper(subTree: CodeTree, remBits: List[Bit]): List[Char] = subTree match
      case Leaf(char, weight) => 
        if remBits.isEmpty then List(char)
        else char :: decodeHelper(tree, remBits)
      case Fork(left, right, chars, weight) => 
        if remBits.head == 0 then decodeHelper(left, remBits.tail)
        else decodeHelper(right, remBits.tail)
      
    decodeHelper(tree, bits)

  
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = 
    decode(frenchCode, secret)

  decodedSecret

