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
import _root_.scala.collection.mutable


object NNDescent {

    def makeNNDescent(dist: Distance)(data: DenseMatrix[Double], nNeighbors: Int, rngState: Array[Long], maxCandidates: Int = 50, nIters: Int = 10, delta: Double = 0.001, rho: Double = 0.5, rpTreeInit: Boolean = true, leafArray: Option[DenseMatrix[Int]] = None, verbose: Boolean = false): (DenseMatrix[Int], DenseMatrix[Double]) = {

        val nVertices = data.rows
        val currentGraph = Heap(nVertices, nNeighbors)
        val leafArrayD = leafArray.getOrElse(DenseMatrix.zeros[Int](1, 1))
        val indices: Array[Long] = Utils.rejectionSample(nNeighbors, nVertices, rngState)
        val rangeIn1 = (0 until indices.length)




        (0 until nVertices).foreach{ i =>
            rangeIn1.foreach{ j =>
                val y = data(indices(j).toInt, ::).t
                val x = data(i, ::).t
                val d: Double = dist(x, y)
                currentGraph.push(i, d, indices(j).toInt, 1)
                currentGraph.push(indices(j).toInt, d, i, 1)
            }
        }

        val currentGraph2 = Heap(nVertices, nNeighbors)
        val indices2: Array[Long] = Utils.rejectionSample(nNeighbors, nVertices, rngState)
        val rangeIn2 = indices2.indices
        (0 until nVertices).foreach{ i =>
            rangeIn2.foreach{ j =>
                val y = data(indices2(j).toInt, ::).t
                val x = data(i, ::).t
                val d: Double = dist(x, y)
                currentGraph2.push(i, d, indices2(j).toInt, 1)
                currentGraph2.push(indices2(j).toInt, d, i, 1)
            }

        }

        if (rpTreeInit) {

            @annotation.tailrec
            def go1n(n: Int): Unit = {
                if (n < leafArrayD.rows) {
                    @annotation.tailrec
                    def go1i(i: Int): Unit = {
                        if (i < leafArrayD.cols && leafArrayD(n, i) > 0) {
                            @annotation.tailrec
                            def go1j(j: Int): Unit = {
                                if (j < leafArrayD.cols && leafArrayD(n, j) > 0) {
                                    val x = data(leafArrayD(n, i), ::).t
                                    val y = data(leafArrayD(n, j), ::).t
                                    val d: Double = dist(x, y)
                                    currentGraph.push(leafArrayD(n, i), d, leafArrayD(n, j), 1)
                                    currentGraph.push(leafArrayD(n, j), d, leafArrayD(n, i), 1)
                                    go1j(j + 1)
                                }
                                else Unit
                            }
                            go1j(i + 1)
                            go1i(i + 1)
                        }
                        else Unit
                    }
                    go1i(0)
                    go1n(n + 1)
                }
                else Unit
            }

            go1n(0)

        }

        @annotation.tailrec
        def go21(n: Int): Unit = {
            
            if (n < nIters) {
                val candidateNeighbors = currentGraph.buildCandidates(nVertices, nNeighbors, maxCandidates, rngState)
                
                @annotation.tailrec
                def go2i(i: Int, sumCi: Int): Int = {
                    
                    if (i < nVertices) {
                        
                        @annotation.tailrec
                        def go2j(j: Int, sumCj: Int): Int = {
                          
                            if (j < maxCandidates) {
                                val p = candidateNeighbors.indices(i, j)
                                
                                if (p < 0 || Utils.tauRand(rngState) < rho) {
                                    go2j(j + 1, sumCj)
                                }
                                else {
                                 
                                    @annotation.tailrec
                                    def go2k(k: Int, sumCk: Int): Int = {
                                        if (k < maxCandidates) {
                                            val q = candidateNeighbors.indices(i, k)
                                            if (q < 0 || (candidateNeighbors.flags(i, j) == 0) && (candidateNeighbors.flags(i, k) == 0)) {
                                                go2k(k + 1, sumCk)
                                            }
                                            else {
                                                val d: Double = dist(data(p, ::).t, data(q, ::).t)
                                                val sum = sumCk + currentGraph.push(p, d, q, 1) + currentGraph.push(q, d, p, 1)
                                                go2k(k + 1, sum)
                                            }
                                        }
                                        else sumCk
                                    }
                                    val sumCk = go2k(0, sumCj)
                                    go2j(j + 1, sumCk)
                                }
                            }
                            else sumCj
                        }
                        val sumCj = go2j(0, sumCi)
                        go2i(i + 1, sumCj)
                    }
                    else sumCi
                }
                val c = go2i(0, 0)
                if (c > delta * nNeighbors * nVertices) go21(n + 1) else Unit
            }
            else Unit
        }

        go21(0)

        currentGraph.deheapSort
    }

    type InitFromRandom = (Int, DenseMatrix[Double], DenseMatrix[Double], Heap, Array[Long]) => Unit
    type InitFromTree = (FlatTree, DenseMatrix[Double], DenseMatrix[Double], Heap, Array[Long]) => Unit

    def makeInitialisation(dist: Distance): (InitFromRandom, InitFromTree) = {

        val initFromRandom: InitFromRandom = (nn, data, qp, h, rngState) => {
            @annotation.tailrec
            def go(i: Int): Unit = {

                if (i < qp.rows) {

                    val indices: Array[Long] = Utils.rejectionSample(nn, data.rows, rngState)

                    @annotation.tailrec
                    def go2(j: Int): Unit = {
                        if (j < indices.length) {
                            if (indices(j) < 0) go2(j + 1)
                            else {
                                val x = data(indices(j).toInt, ::).t
                                val y = qp(i, ::).t
                                val d: Double = dist(x, y)
                                h.push(i, d, indices(j).toInt, 1)
                                go2(j + 1)
                            }
                        }
                    }

                    go2(0)
                    go(i + 1)

                }

            }
            go(0)
        }

        val initFromTree: InitFromTree = (tree, data, qp, h, rngState) => {
            @annotation.tailrec
            def goi(i: Int): Unit = {
                if (i < qp.rows) {
                    val indices: DenseVector[Int] = tree.searchFlatTree(qp(i, ::).t, rngState)
                    @annotation.tailrec
                    def goj(j: Int): Unit = {
                        if (j < indices.length) {
                            if (indices(j) < 0) goj(j + 1)
                            else {
                                val x = data(indices(j), ::).t
                                val y = qp(i, ::).t
                                val d: Double = dist(x, y)
                                h.push(i, d, indices(j), 1)
                                goj(j + 1)
                            }
                        }
                    }
                    goj(0)
                    goi(i + 1)
                }
            }

            goi(0)
        }
        (initFromRandom, initFromTree)
    }


    /**
      *
      * @param forest : Forest
      * @param data : DenseMatrix[Double]
      * @param queryPoints : Array[DenseVector[Double]
      * @param nNeighbors : Int
      * @param rngState : Array[Long]
      * @param dist : Distance
      *
      * @return results : Heap
      */

    def initialiseSearch(forest: Forest, data: DenseMatrix[Double], queryPoints: DenseMatrix[Double], nNeighbors: Int, rngState: Array[Long], dist: Distance): Heap = {
        val (initFromRandom, initFromTree) = makeInitialisation(dist)
        val results = Heap(queryPoints.rows, nNeighbors)
        initFromRandom(nNeighbors, data, queryPoints, results, rngState)

        if (forest.trees.nonEmpty) {
            forest.trees.foreach { tree => initFromTree(tree, data, queryPoints, results, rngState) }
        }
        results
    }

    /**
      *
      * @param dist : Distance Can be any distances implemented
      * @param data : DenseMatrix[Double]
      * @param indPtr : ArrayBuffer[Int]
      * @param indices : ArrayBuffer[Int]
      * @param initialization : Heap
      * @param queryPoints : DenseMatrix[Double]
      *
      * @return initilization : Heap
      */
    def makeInitialisedNNDSearch(dist: Distance)(data: DenseMatrix[Double], initialization: Heap, queryPoints: DenseMatrix[Double]): Heap = {
        @annotation.tailrec
        def initNNDSearchRec(i: Int, init: Heap): Heap = {
            if (i < queryPoints.rows) {
                @annotation.tailrec
                def go(tried: mutable.ArrayBuffer[Int]): Unit = {
                    val vertex = init.smallestFlagged(i)
                    vertex match {
                        case -1 => Unit
                        case _ => {
                            val candidates = data(vertex, ::).t.findAll(_ == 0)
                            candidates.foreach { c =>
                                if (c != vertex && c != -1 && ! tried.contains(c)) {
                                    val d = dist(data(c, ::).t, queryPoints(i, ::).t)
                                    init.uncheckedPush(i, d, c, 1)
                                    tried += c
                                }
                            }
                            go(tried.distinct)
                        }
                    }
                }
                val triedArray = initialization.indices(i, ::).t.toArray.distinct
                go(mutable.ArrayBuffer[Int](triedArray: _*))
                initNNDSearchRec(i + 1, init)
            }
            else init
        }
        initNNDSearchRec(0, initialization)
    }
}
