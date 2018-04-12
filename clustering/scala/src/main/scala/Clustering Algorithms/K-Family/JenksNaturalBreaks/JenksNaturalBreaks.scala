package clustering4ever.scala.clustering

import _root_.scala.collection.mutable.ArrayBuffer

object JenksNaturalBreaks
{
  /**
   * @author Beck Gaël
   * Look at https://en.wikipedia.org/wiki/Jenks_natural_breaks_optimization for more details 
   * Return breaks position in the sorted array
   * @param sortedValues : a sorted array
   * @param nbCatPlusOne : number of breaks user desire
   * @return Indexes of breaks in sortedValues sequence
   *
   **/
  def jenksBrks[T](sortedValues: Seq[T], nbCatPlusOne: Int)(implicit num: Numeric[T]) =
  {
    val nbCat = nbCatPlusOne - 1
    val nbValues = sortedValues.size
    var value = 0D
    var v = 0D
    var id = 0
    var i3 = 0
    var i4 = 0

    val matrixIter = (0 until nbValues).toArray
    val matrixIter2 = (0 until nbCat).toArray
    val mat1 = for( i <- matrixIter ) yield ( for( j <- matrixIter2 ) yield 1D )
    val mat2 = for( i <- matrixIter ) yield ( for( j <- matrixIter2 ) yield Double.MaxValue )
    
    for( l <- 2 to nbValues)
    {
      var s1 = 0D
      var s2 = 0D
      var w = 0D
      for( m <- 1 to l )
      {
        val i3 = l - m + 1
        value = num.toDouble(sortedValues(i3 - 1))
        s2 += value * value
        s1 += value
        w += 1
        v = s2 - (s1 * s1) / w
        i4 = i3 - 1
        if( i4 != 0 )
        {
          for( j <- 2 to nbCat )
          {
            if( mat2(l - 1)(j - 1) >= (v + mat2(i4 - 1)(j - 2)) )
            {
              mat1(l - 1)(j - 1) = i3
              mat2(l - 1)(j - 1) = v + mat2(i4 - 1)(j - 2)
            }
          }
        }
      }
     
      mat1(l - 1)(0) = 1
      mat2(l - 1)(0) = v
    }
          
    val kclass = for( i <- (1 to nbCat).toArray ) yield( i.toDouble )

    kclass(nbCat - 1) = nbValues
    var k = nbValues
    
    for( j <- (2 to nbCat).reverse )
    {
      id = (mat1(k - 1)(j - 1)).toInt - 1
      kclass(j - 2) = id
      k = id
    }

    val res = ArrayBuffer.empty[T]
    res += sortedValues.head

    for( i <- 1 to nbCat )
    {
      res += sortedValues(kclass(i - 1).toInt - 1)
    }
    
    res
  }
}
