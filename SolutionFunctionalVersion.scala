//
// Author: Tomasz Sadowski
//

// ----------------------------------------------------------------------------------------------------
//
// Solution based on Manacher's 1975 LPS (Longest palindromic substring) algorithm
// My modifications:
//         - do not allocate memory for working array containing input string with '#' after each char
//         - count number of palindromes while iterating over working array. update total number of
//           palindromes
//
object Solution {

    def solution(s: String): Int = {
        val p = findPalindromes(s)
        calculatePalindromesCount(p)
    }

    // ----------------------------------------------------------------------------------------------------
    //
    // calculate array with subpalindroms radii on i-th position using Longest Palindromic Substring
    // algorithm
    //
    def findPalindromes(s: String): Array[Int] = {
        var center, right, mirror = 0
        var palindromesCount = 0
        val p = new Array[Int](2*s.length + 2)  // contains number of palindromes at i-th position.
                                                // initialized implicitly with zeroes
        for(i <- 1 to 2*s.length + 1) {
            mirror = 2 * center - i

            if (i < right)
                p(i) = Math.min(right - i, p(mirror))

            while (getChar(s, i + (p(i) + 1)) == getChar(s, i - (p(i) + 1)))
                p(i) += 1

            if (i + p(i) > right) {
                center = i
                right = i + p(i)
            }
        }
        p
    }

    // -------------------------------------------------------------------------------------------
    //
    // get char of 'modified string' in following format: i.e. aba -> ^#a#b#a#$
    // Index parameter is position of char which we would like to get from 'modified string'
    // This approach save up memory as it does not require memory allocation for 'modified string'
    //
    def getChar(s: String, index: Int): Char = index match {
        case 0                                  => '^'
        case index if index == s.length * 2 + 2 => '$'
        case index if index % 2 == 0            => s((index / 2) - 1)
        case _                                  => '#'
    }

    // -------------------------------------------------------------------------------------------
    //
    // calculate number of all palindromes based on array with subpalindroms radii
    //
    def calculatePalindromesCount(p: Array[Int]): Int = {
        val MaxPalindromesCount = 100000000

        // calculate number of palindromes which is half of p(i)
        val palindromesCount =  p.fold(0) { (z, i) => z + (i / 2) }

        // if number palindromes exceeds some high number then return -1
        if( palindromesCount > MaxPalindromesCount ) -1 else palindromesCount
    }

    def main(args: Array[String]): Unit = {
        println( solution("baababa") )
    }
}