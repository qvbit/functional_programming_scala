type Word = String
type Occurrence = (Char, Int)
type Occurrences = List[Occurrence]
type Sentence = List[Word]

def wordOccurrences(w: Word): Occurrences = 
    w.groupBy(x => x.toLower).mapValues(x => x.length).toList.sorted


val word = "ggggfooBaRro"
wordOccurrences(word)

def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences(s.mkString)

val s = List("Scala", "is", "cool")
sentenceOccurrences(s)


// /** In Python, the code is:

// def combinations(l):
//     ret = [[]]
    
//     for c, cnt in l:
//         level = []
//         for curr in ret:
//             for i in range(1, cnt+1):
//                 level.append(curr + [(c, i)])
//         ret += level
    
//     return ret

// */

def combinations(occurrences : Occurrences): List[Occurrences] = occurrences match
    case List() => List(List())
    case x :: xs =>
        val prevResults = combinations(xs)
        (for
            curr <- generateCurr(x)
        yield
            prevResults ::: prevResults.map(prev => curr :: prev)).flatten.distinct


    def generateCurr(occurrence: Occurrence): Occurrences = occurrence match
        case (char, count) =>
            (for i <- (1 to count)
            yield (char, i)).toList

combinations( List(('a', 2), ('b', 2)) )

def subtract(x: Occurrences, y: Occurrences): Occurrences = y match
    case List() => x
    case yHead :: yTail => subtract(subtractHelper(x, yHead), yTail)

    def subtractHelper(x: Occurrences, occ: Occurrence): Occurrences =
        val (xChar, xCount) = x.head
        val (yChar, yCount) = occ

        if xChar == yChar && xCount == yCount then x.tail // If equal, we just get rid of this occurrence
        else if xChar == yChar then (xChar, xCount - yCount) :: x.tail  // Subtract yCount from xCount and return
        else x.head :: subtractHelper(x.tail, occ)  // o.w. recurs and process rest of x until you found matching char

val x = List(('a', 1), ('d', 1), ('f', 2), ('g', 4))
val y = List(('a', 1), ('f', 1), ('g', 2))

subtract(x, y)


lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupby(w => wordOccurrences(w))
    
def sentenceAnagrams(sentence: Sentence): List[Sentence] =
    def recurse(occs: Occurrences): List[Sentence] =
        if occs.isEmpty then List(List())
        else
            for
                subset <- combinations(occs)
                word <- dictionaryByOccurrences(subset)
            yield
                word :: recurse(subtract(occs, subset))

    recurse(sentenceOccurrences(sentence))

sentenceAnagrams(List("I", "love", "you"))