package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import org.clustering4ever.roottraits.{Clusterizable, GVector}

import scala.collection.{GenSeq, mutable, parallel}
import scala.language.higherKinds
/**
 *
 */
object SortingTools extends Serializable {
	/**
	 *
	 */
	final def sortByID[O, V <: GVector[V], Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]) = {
        val sorted = data.seq.sortBy(_.id)
        val builder = data.genericBuilder[Cz[O, V]].asInstanceOf[mutable.Builder[Cz[O, V], GS[Cz[O, V]]]]
        builder.sizeHint(data.size)
        builder ++= sorted
        builder.result
    }
    /**
     *
     */
    final def radixSort(toSort: Array[Int]): Array[Int] = {
        var arr = toSort
        for (shift <- Integer.SIZE - 1 until -1 by -1) {
            val tmp = new Array[Int](arr.length)
            var j = 0
            for (i <- arr.indices) {
                if ((shift == 0) == (arr(i) << shift >= 0)) arr(i - j) = arr(i)
                else {
                    tmp(j) = arr(i)
                    j += 1
                }
            }
            arr.copyToArray(tmp, j, arr.length - j)
            arr = tmp
        }
        arr
    }
    /**
     * @param arr the input array of double
     * @param b the number of buckets used which also defined parallelism
     * Advise, faster than quickSort
     */
    final def bucketSort(toSort: Array[Double], b: Int) = {
      val buckets = parallel.mutable.ParArray.fill(b)(mutable.ArrayBuffer.empty[Double])
      val m = toSort.max
      @annotation.tailrec
      def go(i: Int) : Unit = {
        if(i < toSort.length) {
            buckets((toSort(i) / m * (b - 1)).toInt) += toSort(i)
            go(i + 1)
        }
      }
      go(0)
      buckets.flatMap(_.sorted)
    }

}