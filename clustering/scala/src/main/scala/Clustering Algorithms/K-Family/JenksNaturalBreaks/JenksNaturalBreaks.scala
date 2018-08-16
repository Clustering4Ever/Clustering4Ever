package clustering4ever.scala.clustering

import scala.collection.{mutable, GenSeq}

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
  def jenksBrks[T](sortedValues: GenSeq[T], nbCatPlusOne: Int)(implicit num: Numeric[T]) =
  {
    val nbCat = nbCatPlusOne - 1
    val nbValues = sortedValues.size
    var value = 0D
    var v = 0D
    var i3 = 0
    var i4 = 0

    val matrixIter = (0 until nbValues).toArray
    val matrixIter2 = (0 until nbCat).toArray
    val mat1 = for( i <- matrixIter ) yield for( j <- matrixIter2 ) yield 1D
    val mat2 = for( i <- matrixIter ) yield for( j <- matrixIter2 ) yield Double.MaxValue
    
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
          
    val kclass = (1 to nbCat).map(_.toDouble).toArray

    kclass(nbCat - 1) = nbValues

    def update(j: Int, kkclass: (Int, Array[Double])) =
    {
      val k = kkclass._1
      val kclass = kkclass._2
      val id = (mat1(k - 1)(j - 1)).toInt - 1
      kclass(j - 2) = id
      (id, kclass)      
    }

    @annotation.tailrec
    def go(n: Int, kkclass: (Int, Array[Double])): (Int, Array[Double]) =
    {
      if( n > 2 ) go(n - 1, update(n, kkclass))
      else update(n, kkclass)
    }

    val (_, kclass2) = go(nbCat, (nbValues, kclass))

    val res = mutable.ArrayBuffer.empty[T]
    res += sortedValues.head

    (1 to nbCat).foreach( i => res += sortedValues(kclass2(i - 1).toInt - 1) )
    
    res.toVector
  }
}
