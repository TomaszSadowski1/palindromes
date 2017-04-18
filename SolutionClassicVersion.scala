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
        var center, right, mirror = 0
        var palindromesCount = 0
        val MaxPalindromesCount = 100000000
        val p = new Array[Int](2*s.length + 2)  // contains number of palindromes at i-th position.
                                                // initialized implicitly with zeroes
        // algorithm main loop
        for(i <- 1 to 2*s.length + 1) {
            mirror = 2*center - i

            if( i < right )
                p(i) = Math.min( right - i, p(mirror) )

            while (getChar(s, i + (p(i) + 1)) == getChar(s, i - (p(i) + 1)))
                p(i) += 1

            if( i + p(i) > right ) {
                center = i
                right = i + p(i)
            }

            // calculate current number of palindromes which is half of p(i)
            palindromesCount += p(i) / 2

            // detect too many palindromes in main loop allows for earlier end of application
            // other option would be to do it outside of main loop using i.e. fold but
            // would be little slower
            if( palindromesCount > MaxPalindromesCount )
                -1
        }

        palindromesCount
    }

    // -------------------------------------------------------------------------------------------
    //
    // get char of 'modified string' in following format: i.e. aba -> ^#a#b#a#$
    // Index parameter is position of char which we would like to get from 'modified string'
    // This approach save up memory as it does not require memory allocation for 'modified string'
    //
    def getChar(s: String, index: Int): Char = {
        if( index == 0 )
            return '^'
        else if( index == s.length * 2 + 2 )
            return '$'
        else if( index % 2 == 0 )
            return s( (index/2)-1 )
        else return '#'
    }

    def main(args: Array[String]): Unit = {
        println( solution("baababa") )
    }
}

