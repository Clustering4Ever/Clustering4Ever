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
import breeze.linalg._
import breeze.numerics.Inf


final case class Heap(final val nPoints: Int, final val size: Int) {
    val indices: DenseMatrix[Int] = DenseMatrix.fill(nPoints, size)(-1)
    val weights: DenseMatrix[Double] = DenseMatrix.fill(nPoints, size)(Inf)
    val flags: DenseMatrix[Int] = DenseMatrix.fill(nPoints, size)(0)

    override def toString: String = {
        indices.toString + "\n" + weights.toString + "\n" + flags.toString
    }

    final def push(row: Int, weight: Double, index: Int, flag: Int): Int = {
        val ind = indices(row, ::).t
        val wei = weights(row, ::).t
        val isN = flags(row, ::).t


        def contains(v: DenseVector[Int], nb: Int) = {
            @annotation.tailrec
            def containsRec(dv: DenseVector[Int], cont: Boolean, i: Int): Boolean = {
                if (i < dv.length) {
                    if (dv(i) == nb) true
                    else containsRec(dv, false, i + 1)
                }
                else cont
            }
            containsRec(v, false, 0)
        }

        if (weight >= wei(0)) 0
        else if (contains(ind, index)) 0
        else {
            wei(0) = weight
            ind(0) = index
            isN(0) = flag

            @annotation.tailrec
            def pushRec(i: Int): Int = {
                val ic1 = 2 * i + 1
                val ic2 = ic1 + 1

                if (ic1 < size) {
                    if (ic2 >= size) {
                        if (wei(ic1) > weight) {
                            wei(i) = wei(ic1)
                            ind(i) = ind(ic1)
                            isN(i) = isN(ic1)
                            pushRec(ic1)
                        }
                        else i
                    }
                    else if (wei(ic1) >= wei(ic2)) {
                        if (wei(ic1) > weight) {
                            wei(i) = wei(ic1)
                            ind(i) = ind(ic1)
                            isN(i) = isN(ic1)
                            pushRec(ic1)
                        }
                        else i
                    }
                    else {
                        if (weight < wei(ic2)) {
                            wei(i) = wei(ic2)
                            ind(i) = ind(ic2)
                            isN(i) = isN(ic2)
                            pushRec(ic2)
                        }
                        else i
                    }
                }
                else i
            }

            val i = pushRec(0)

            indices(row, i) = index
            weights(row, i) = weight
            flags(row, i) = flag

            1
        }
    }

     final def uncheckedPush(row: Int, weight: Double, index: Int, flag: Int): Int = {
         val ind = indices(row, ::).t
         val wei = weights(row, ::).t
         val isN = flags(row, ::).t


         def contains(v: DenseVector[Int], nb: Int) = {
             @annotation.tailrec
             def containsRec(dv: DenseVector[Int], cont: Boolean, i: Int): Boolean = {
                 if (i < dv.length) {
                     if (dv(i) == nb) true
                     else containsRec(dv, false, i + 1)
                 }
                 else cont
             }
             containsRec(v, false, 0)
         }

         if (weight >= wei(0)) 0
         else {
             wei(0) = weight
             ind(0) = index
             isN(0) = flag

             @annotation.tailrec
             def pushRec(i: Int): Int = {
                 val ic1 = 2 * i + 1
                 val ic2 = ic1 + 1

                 if (ic1 < size) {
                     if (ic2 >= size) {
                         if (wei(ic1) > weight) {
                             wei(i) = wei(ic1)
                             ind(i) = ind(ic1)
                             isN(i) = isN(ic1)
                             pushRec(ic1)
                         }
                         else i
                     }
                     else if (wei(ic1) >= wei(ic2)) {
                         if (wei(ic1) > weight) {
                             wei(i) = wei(ic1)
                             ind(i) = ind(ic1)
                             isN(i) = isN(ic1)
                             pushRec(ic1)
                         }
                         else i
                     }
                     else {
                         if (weight < wei(ic2)) {
                             wei(i) = wei(ic2)
                             ind(i) = ind(ic2)
                             isN(i) = isN(ic2)
                             pushRec(ic2)
                         }
                         else i
                     }
                 }
                 else i
             }

             val i = pushRec(0)

             indices(row, i) = index
             weights(row, i) = weight
             flags(row, i) = flag

             1
         }
     }

    /**
      * Search the heap for the smallest element that is
      * still flagged.
      *
      * @param row Which of the heaps to search
      * @return The index of the smallest flagged element
      *         of the ``row``th heap, or -1 if no flagged
      *         elements remain in the heap.
      */
    final def smallestFlagged(row: Int): Int = {
        val inds = indices(row, 0 until size).t
        val weig = weights(row, 0 until size).t
        val isNe = flags(row, 0 until size).t

        val ind: Array[Int] = inds.toArray
        val dist: Array[Double] = weig.toArray
        val flag: Array[Int] = isNe.toArray


        @annotation.tailrec
        def go(i: Int, md: Double, ri: Int): (Double, Int) = {
            if (i < ind.indices.length) {
                if ((flag(i) == 1) && (dist(i) < md)) {
                    go(i + 1, dist(i), i)
                }
                else go(i + 1, md, ri)
            }
            else (md, ri)
        }
        val (minDist, resultIndex) = go(0, Inf, -1)

        if (resultIndex >= 0) {
            flags(row, resultIndex) = 0
            ind(resultIndex)
        }
        else {
            -1
        }
    }


    /** Build a heap of candidate neighbors for nearest neighbor descent. For
      * each vertex the candidate neighbors are any current neighbors, and any
      * vertices that have the vertex as one of their nearest neighbors.
      *
      * @param nVertices The total number of vertices in the graph.
      * @param nNeighbors The number of neighbor edges per node in the current graph.
      * @param maxCandidates The maximum number of new candidate neighbors.
      * @param rngState The internal state of the rng
      * @return candidateNeighbors: A heap with an array of (randomly sorted) candidate
      *         neighbors for each vertex in the graph.
      */

    final def buildCandidates(nVertices: Int, nNeighbors: Int, maxCandidates: Int, rngState: Array[Long]): Heap = {

        @annotation.tailrec
        def go(i: Int, cn: Heap): Heap = {
            if(i < nVertices) {
                @annotation.tailrec
                def go2(j: Int, h: Heap): Heap = {
                    if(j < nNeighbors) {
                        if(indices(i, j) < 0)
                            go2(j + 1, h)
                        else {
                            val idx = indices(i, j)
                            val isn = flags(i, j)
                            val d = Utils.tauRand(rngState)
                            h.push(i, d, idx, isn)
                            h.push(idx, d, i, isn)
                            flags(i, j) = 0
                            go2(j + 1, h)
                        }
                    } else h
                }
                val candidateNeighbors = go2(0, cn)
                go(i + 1, candidateNeighbors)
            } else cn
        }
        go(0, Heap(nVertices, maxCandidates))
    }

    /** Build a heap of candidate neighbors for nearest neighbor descent. For
      * each vertex the candidate neighbors are any current neighbors, and any
      * vertices that have the vertex as one of their nearest neighbors.
      *
      * @param nVertices The total number of vertices in the graph.
      * @param nNeighbors The number of neighbor edges per node in the current graph.
      * @param maxCandidates The maximum number of new candidate neighbors.
      * @param rngState The internal state of the rng
      * @param rho
      * @return A heap with an array of (randomly sorted) candidate
      *         neighbors for each vertex in the graph.
      */
    final def newBuildCandidates(nVertices: Int, nNeighbors: Int, maxCandidates: Int, rngState: Array[Long], rho: Double = 0.5): (Heap, Heap) = {

        @annotation.tailrec
        def go(i: Int, ncn: Heap, ocn: Heap): (Heap, Heap) = {
            if(i < nVertices) {
                @annotation.tailrec
                def go2(j: Int, th: (Heap, Heap)): (Heap, Heap) = {
                    if(j < nNeighbors && indices(i,j) > 0) {
                        val idx = indices(i, j)
                        val isn = flags(i, j)
                        val d = Utils.tauRand(rngState)
                        if(Utils.tauRand(rngState) < rho) {
                            var c = 0
                            if (isn == 1) {
                                c += th._1.push(i, d, idx, isn)
                                c += th._2.push(idx, d, i, isn)
                            }
                            else {
                                th._1.push(i, d, idx, isn)
                                th._2.push(idx, d, i, isn)
                            }

                            if (c > 0) {
                                flags(i, j) = 0
                            }
                        }
                        go2(j + 1, th)
                    } else th
                }
                val (newCandidateNeighbors, oldCandidateNeighbors) = go2(0, (ncn, ocn))
                go(i + 1, newCandidateNeighbors, oldCandidateNeighbors)
            } else (ncn, ocn)
        }

        go(0, Heap(nVertices, maxCandidates), Heap(nVertices, maxCandidates))
    }

    /** Given an array of heaps (of indices and weights), unpack the heap
      * out to give and array of sorted lists of indices and weights by increasing
      * weight. This is effectively just the second half of heap sort (the first
      * half not being required since we already have the data in a heap).
      *
      * @return The indices and weights sorted by increasing weight.
      */

    final def deheapSort: (DenseMatrix[Int], DenseMatrix[Double]) = {

        val nbCols = indices.cols
        val nbRows = indices.rows
        @annotation.tailrec
        def deheapSortRec(i: Int, indHeap: DenseMatrix[Int], weightHeap: DenseMatrix[Double]):
        (DenseMatrix[Int], DenseMatrix[Double]) = {

            if (i < nbRows) {

                @annotation.tailrec
                def go(j: Int, indVec: DenseVector[Int], weightVec: DenseVector[Double]):
                (DenseVector[Int], DenseVector[Double]) = {

                    if (j < nbCols - 1) {
                        val tInd: Int Tuple2 Int = Tuple2(indVec(0), indVec(indVec.length - j - 1))
                        val tDist: Double Tuple2 Double = Tuple2(weightVec(0), weightVec(weightVec.length - j - 1))
                        val tIndSwap = tInd.swap
                        val tDistSwap = tDist.swap
                        indVec(0) = tIndSwap._1
                        indVec(indVec.length - j - 1) = tIndSwap._2
                        weightVec(0) = tDistSwap._1
                        weightVec(weightVec.length - j - 1) = tDistSwap._2

                        val distFirsts = weightVec(0 until weightVec.length - j - 1)
                        val indFirsts = indVec(0 until indVec.length - j - 1)
                        Utils.siftdown(distFirsts, indFirsts, 0)
                        go(j + 1, indVec, weightVec)
                    }
                    else (indVec, weightVec)
                }

                val (indHeapVec,weightHeapVec) =  go(0, indHeap(i, ::).t.copy, weightHeap(i, ::).t.copy)
                indHeap(i, ::) := indHeapVec.t
                weightHeap(i, ::) := weightHeapVec.t
                deheapSortRec(i + 1, indHeap.copy, weightHeap.copy)
            }
            else (indHeap, weightHeap)
        }

        deheapSortRec(0, indices, weights)
    }

}
