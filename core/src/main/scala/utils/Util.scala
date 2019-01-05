package clusering4ever.util
/**
 *
 */
object Util {
  /**
   * Determine in which interval falls a value given a specific range.
   * @return i if smaller or equal than ith value (starting at 0) and range.size +1 if bigger than the last range value 
   */
  final def whichInterval[N](d: N, range: Seq[N])(implicit num: Numeric[N]) = {
    @annotation.tailrec
    def go(i: Int): Int = {
      if(num.lteq(d, range(i))) i
      else if(num.gt(d, range.last)) range.size + 1
      else go(i + 1)
    }
    go(0)
  }
}