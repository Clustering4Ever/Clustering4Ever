package org.clustering4ever.scala.umap
/**
 * @author Beugnet Vincent
 * @author Hurvois Guillaume
 * @author Ladjal Adlane
 * @author Merien Grégoire
 * @author Serfas Florent
 * @author Beck Gaël
 * @author Forest Florent
 */
import breeze.linalg.{DenseMatrix, DenseVector}
import _root_.scala.collection.mutable
/**
 *
 */
object Utils extends Serializable {

    def mod(a: Int, b: Int): Int = {
        val res = a % b
        if (res < 0) res + b
        else res
    }

    /**
      *
      * @param state The Longernal state of the rng
      * @return A (pseudo)-random Int value
      */
    def tauRandInt(state: Array[Long]) : Int = {
        state(0) = (((state(0) & 0xfffffffeL) << 12) & 0xffffffffL) ^ (
            (((state(0) << 13) & 0xffffffffL) ^ state(0)) >> 19
            )
        state(1) = (((state(1) & 0xfffffff8L) << 4) & 0xffffffffL) ^ (
            (((state(1) << 2) & 0xffffffffL) ^ state(1)) >> 25
            )
        state(2) = (((state(2) & 0xfffffff0L) << 17) & 0xffffffffL) ^ (
            (((state(2) << 3) & 0xffffffffL) ^ state(2)) >> 11
            )

        (state(0) ^ state(1) ^ state(2)).toInt
    }

    /**
      * A (pseudo)-random number generator for floats in the range [0,1]
      * @param state The Longernal state of the rng
      * @return A (pseudo)-random float in the Longerval [0, 1]
      */

    def tauRand(state : Array[Long]) : Float = {
        val longeger : Int = tauRandInt(state)
        longeger.toFloat / Int.MaxValue
    }


    /** Generate nSamples many Longegers from 0 to poolSize such that no
      * Longeger is selected twice. The duplication constraLong is achieved via
      * rejection sampling.
      *
      * @param nSamples The number of random samples to select from the pool
      * @param poolSize The size of the total pool of candidates to sample from
      * @param rngState Longernal state of the random number generator
      * @return The ``nSamples`` randomly selected elements from the pool.
      */
    def rejectionSample(nSamples: Int, poolSize: Int, rngState: Array[Long]): Array[Long] = {

        @annotation.tailrec
        def goRejectSample(i: Int, res: mutable.ArrayBuffer[Long]): mutable.ArrayBuffer[Long] = {
            if (i < nSamples) {
                @annotation.tailrec
                def chooseCandidate: Long = {
                    val j = mod(tauRandInt(rngState), poolSize)
                    if (res.contains(j)) chooseCandidate
                    else j
                }
                goRejectSample(i + 1, res += chooseCandidate)
            }
            else res
        }

        goRejectSample(0, new mutable.ArrayBuffer[Long](nSamples)).toArray[Long]
    }


    /** Restore the heap property for a heap with an out of place element
      * at position ``elt``. This works with a heap pair where arr1 carries
      * the weights and arr2 holds the corresponding elements.
      *
      * @param arr1 : DenseVector[Double] Heap carrying the weights
      * @param arr2 : DenseVector[Int] Heap holding the corresponding elements
      */

    def siftdown(arr1: DenseVector[Double], arr2: DenseVector[Int], elt: Int): Unit = {
        /*var eltIsEqualSwap = false*/

        @annotation.tailrec
        def siftdownRec(eltR: Int): Unit = {
            if (eltR * 2 + 1 < arr1.length) {
                val leftChild = eltR * 2 + 1
                val rightChild = leftChild + 1
                var swap = eltR

                if (arr1(swap) < arr1(leftChild))
                    swap = leftChild

                if (rightChild < arr1.length && arr1(swap) < arr1(rightChild))
                    swap = rightChild

                if (swap != eltR) {
                    val t1 = Tuple2(arr1(eltR), arr1(swap))
                    val t2 = Tuple2(arr2(eltR), arr2(swap))
                    val t1swap = t1.swap
                    val t2swap = t2.swap
                    arr1(eltR) = t1swap._1
                    arr1(swap) = t1swap._2
                    arr2(eltR) = t2swap._1
                    arr2(swap) = t2swap._2
                    siftdownRec(swap)
                }
            }
        }
        siftdownRec(elt)
    }

    /**
      *
      * @param dmat Original matrix.
      * @param indicesCol Indices to keep. Each row consists of the indices of the columns.
      * @param nNeighbors Number of neighbors.
      * @return The corresponding submatrix.
      */
    def submatrix(dmat: Array[Array[Double]], indicesCol: Array[Array[Int]], nNeighbors: Int): Array[Array[Double]] = {
        val nSamplesTransform = dmat.length
        val submat: Array[Array[Double]] = Array.fill(nSamplesTransform, nNeighbors)(0)
        
        val range2 = (0 until nNeighbors)//.par
        
        (0 until nSamplesTransform)/*.par*/.foreach{ i =>
          range2.foreach{ j =>
            submat(i)(j) = dmat(i)(indicesCol(i)(j))
          }
        }

        submat

    }

    def makeMatrix(rows: DenseVector[Int], cols: DenseVector[Int], vals: DenseVector[Double], rowDim: Int, colDim: Int) = {
        val result = DenseMatrix.zeros[Double](rowDim, colDim)
        (0 until rows.length).foreach( i => result(rows(i), cols(i)) = result(rows(i), cols(i)) + vals(i) )
        result
    }
}
