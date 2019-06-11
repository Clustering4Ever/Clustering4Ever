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
import scala.collection.mutable


object NNDescent {

    def makeNNDescent(dist: Distance)
                     (data: DenseMatrix[Double], nNeighbors: Int, rngState: Array[Long], maxCandidates: Int = 50, nIters: Int = 10,
                      delta: Double = 0.001, rho: Double = 0.5, rpTreeInit: Boolean = true, leafArray: Option[DenseMatrix[Int]] = None, verbose: Boolean = false): (DenseMatrix[Int], DenseMatrix[Double]) = {

        val nVertices = data.rows
        val currrentGraph = Heap(nVertices, nNeighbors)
        val leafArrayD = leafArray.getOrElse(DenseMatrix.zeros[Int](1, 1))

        @annotation.tailrec
        def go1(i: Int): Unit = {
            if (i < nVertices) {
                val indices: Array[Long] = Utils.rejectionSample(nNeighbors, nVertices, rngState)

                @annotation.tailrec
                def go2(j: Int): Unit = {
                    if (j < indices.length) {
                        val y = data(indices(j).toInt, ::).t
                        val x = data(i, ::).t
                        val d: Double = dist(x, y)
                        currrentGraph.push(i, d, indices(j).toInt, 1)
                        currrentGraph.push(indices(j).toInt, d, i, 1)
                        go2(j + 1)
                    }
                }

                go2(0)
                go1(i + 1)
            }
        }

        go1(0)

        // Version simplifee
        val currrentGraph2 = Heap(nVertices, nNeighbors)
        val indices2: Array[Long] = Utils.rejectionSample(nNeighbors, nVertices, rngState)
        val r2 = indices2.indices
        (0 until nVertices).foreach{ i =>
            r2.foreach{ j =>
                val y = data(indices2(j).toInt, ::).t
                val x = data(i, ::).t
                val d: Double = dist(x, y)
                currrentGraph2.push(i, d, indices2(j).toInt, 1)
                currrentGraph2.push(indices2(j).toInt, d, i, 1)
            }

        }

        // Check résultat des 2 versions
        // require(currrentGraph == currrentGraph2, println("il y a probablement une petite betise caché :)"))

        if (rpTreeInit) {

            @annotation.tailrec
            def go11(n: Int): Unit = {
                if (n < leafArrayD.rows) {

                    @annotation.tailrec
                    def go12(i: Int): Unit = {
                        if (i < leafArrayD.cols && leafArrayD(n, i) > 0) {

                            @annotation.tailrec
                            def go13(j: Int): Unit = {
                                if (j < leafArrayD.cols && leafArrayD(n, j) > 0) {
                                    val x = data(leafArrayD(n, i), ::).t
                                    val y = data(leafArrayD(n, j), ::).t
                                    val d: Double = dist(x, y)
                                    currrentGraph.push(leafArrayD(n, i), d, leafArrayD(n, j), 1)
                                    currrentGraph.push(leafArrayD(n, j), d, leafArrayD(n, i), 1)
                                    go13(j + 1)
                                }
                            }

                            go13(i + 1)
                            go12(i + 1)
                        }
                    }

                    go12(0)
                    go11(n + 1)
                }
            }

            go11(0)
        }

        @annotation.tailrec
        def go21(n: Int): Unit = {
            if (n < nIters) {
                if (verbose) {
                    println("\t" + n + "\t / \t" + nIters)
                }
                val candidateNeighbors = currrentGraph.buildCandidates(nVertices, nNeighbors, maxCandidates, rngState)
                var c: Int = 0
                @annotation.tailrec
                def go22(i: Int): Unit = {
                    if (i < nVertices) {
                        @annotation.tailrec
                        def go23(j: Int): Unit = {
                            if (j < maxCandidates) {
                                val p = candidateNeighbors.indices(i, j)
                                if (p < 0 || Utils.tauRand(rngState) < rho) {
                                    go23(j + 1)
                                }
                                else {
                                    @annotation.tailrec
                                    def go24(k: Int): Unit = {
                                        if (k < maxCandidates) {
                                            val q = candidateNeighbors.indices(i, k)
                                            if (q < 0 || (candidateNeighbors.flags(i, j) == 0) && (candidateNeighbors.flags(i, k) == 0)) {
                                                go24(k + 1)
                                            }
                                            else {
                                                val d: Double = dist(data(p, ::).t, data(q, ::).t)
                                                c += currrentGraph.push(p, d, q, 1)
                                                c += currrentGraph.push(q, d, p, 1)
                                                go24(k + 1)
                                            }
                                        }
                                    }

                                    go24(0)
                                    go23(j + 1)
                                }
                            }
                        }

                        go23(0)
                        go22(i + 1)
                    }
                }

                go22(0)
                if (c > delta * nNeighbors * nVertices)
                    go21(n + 1)
            }
        }

        go21(0)
        val res = currrentGraph.deheapSort
        res
    }

    def makeInitialisation(dist: Distance) = {

        val initFromRandom: (Int, DenseMatrix[Double], DenseMatrix[Double], Heap, Array[Long]) => Unit
        = (nn, data, qp, h, rngState) => {
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


        val initFromTree: (FlatTree, DenseMatrix[Double], DenseMatrix[Double], Heap, Array[Long]) => Unit
        = (tree, data, qp, h, rngState) => {
            @annotation.tailrec
            def go(i: Int): Unit = {
                if (i < qp.rows) {
                    val indices: DenseVector[Int] = tree.searchFlatTree(qp(i, ::).t, rngState) //TODO
                    @annotation.tailrec
                    def go2(j: Int): Unit = {
                        if (j < indices.length) {
                            if (indices(j) < 0) go2(j + 1)
                            else {
                                val x = data(indices(j), ::).t
                                val y = qp(i, ::).t
                                val d: Double = dist(x, y)
                                h.push(i, d, indices(j), 1)
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
        Tuple2(initFromRandom, initFromTree)
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

    def initialiseSearch(forest: Forest, data: DenseMatrix[Double], queryPoints: DenseMatrix[Double],
                         nNeighbors: Int, rngState: Array[Long], dist: Distance): Heap = {
        val (initFromRandom, initFromTree) = makeInitialisation(dist)
        val results = Heap(queryPoints.rows, nNeighbors)
        initFromRandom(nNeighbors, data, queryPoints, results, rngState)

        if (forest.trees.nonEmpty) {
            forest.trees.foreach { tree =>
                initFromTree(tree, data, queryPoints, results, rngState)
            }
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

    def makeInitialisedNNDSearch(dist: Distance)
                                (data: DenseMatrix[Double], initialization: Heap, queryPoints: DenseMatrix[Double]): Heap = {
        @annotation.tailrec
        def initNNDSearchRec(i: Int, init: Heap): Heap = {
            if (i < queryPoints.rows) {
                @annotation.tailrec
                def go(tried: mutable.ArrayBuffer[Int]): mutable.ArrayBuffer[Int] = {
                    val vertex = init.smallestFlagged(i)
                    vertex match {
                        case -1 => tried
                        case _ =>
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
                val triedArray = initialization.indices(i, ::).t.toArray.distinct
                go(mutable.ArrayBuffer[Int](triedArray: _*))
                initNNDSearchRec(i + 1, init)
            }
            else init
        }
        initNNDSearchRec(0, initialization)
    }
}
