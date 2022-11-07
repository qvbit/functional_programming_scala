// Note that this is different from leetcode problem because we're given the list of possible
// strings as input. So we're NOT enumerating all combinations but rather trying to make possible 
// number-phrases from a given dictionary of words
class Coder(words: List[String]):
    val mnemonics = Map(
        '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
        '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

    /** Maps a letter to the digit it represents */ 
    private val charCode: Map[Char, Char] = 
        for
            (digit, str) <- mnemonics
            ltr <- str
        yield ltr -> digit

    /** Maps a word to the digit string it can represent */
    private def wordCode(word: String): String = 
        word.toUpperCase.map(charCode)

    /** Maps a digit string to all words in the dictionary (i.e. the parameter words) that represent it*/ 
    private val wordsForNum: Map[String, List[String]] =
        // For each word, take the wordCode (i.e. the digit string that represents the word) and this digit string becomes
        // the key to the output map. The values is a list of strings that have that specific wordCode (digit string)
        words.groupBy(wordCode).withDefaultValue(Nil)


    /** All ways to encode a number as a list of words */
    def encode(number: String): Set[List[String]] =
        if number.isEmpty then Set(Nil)
        else
            for
                splitPoint <- (1 to number.length).toSet // Use set here since collection you start with in for is colection you end with (and we need Set)
                word <- wordsForNum(number.take(splitPoint))  // Gives us back a list of words that correspond to this digit string
                rest <- encode(number.drop(splitPoint))  // Note that if this is empty, we dont execute the yield below and skip this split point. Hence partial phrases are not returned as expected.
            yield word :: rest


val coder = Coder(List(
    "Scala", "Python", "Ruby", "C",
    "rocks", "socks", "sucks", "works", "pack"
))

val number = "7225276257"

val ret = coder.encode(number).map(_.mkString(" "))

ret

for
    word <- List("scala", "malla")
    rest <- List()
yield word