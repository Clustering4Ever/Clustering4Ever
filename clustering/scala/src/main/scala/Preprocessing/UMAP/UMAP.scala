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
import breeze.numerics._
import breeze.stats.distributions.Gaussian
import com.thesamet.spatial.{DimensionalOrdering, KDTree}
import org.apache.commons.math3.analysis.ParametricUnivariateFunction
import org.apache.commons.math3.fitting._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Random

object UMAP {

    val smoothKtolerance = 0.00001
    val minKdistScale = 0.001

    /**
      * Compute a continuous version of the distance to the kth nearest
      * neighbor. That is, this is similar to knn-distance but allows continuous
      * k values rather than requiring an integral k. In esscence we are simply
      * computing the distance such that the cardinality of fuzzy set we generate
      * is k.
      *
      * @param distances         Distances to nearest neighbors for each samples. Each row should be a
      *                          sorted list of distances to a given samples nearest neighbors.
      * @param k                 The number of nearest neighbors to approximate for.
      * @param nIter             We need to binary search for the correct distance value. This is the
      *                          max number of iterations to use in such a search.
      * @param localConnectivity The local connectivity required -- i.e. the number of nearest
      *                          neighbors that should be assumed to be connected at a local level.
      *                          The higher this value the more connected the manifold becomes
      *                          locally. In practice this should be not more than the local intrinsic
      *                          dimension of the manifold.
      * @param bandwith          The target bandwidth of the kernel, larger values will produce
      *                          larger return values.
      * @return The distance to kth nearest neighbor, as suitably approximated.
      *         The distance to the 1st nearest neighbor for each point.
      */
    def smoothKNNDist(distances: DenseMatrix[Double], k: Int,
                      nIter: Int = 64,
                      localConnectivity: Double = 1.0,
                      bandwith: Double = 1.0)
    : DenseVector[Double] Tuple2 DenseVector[Double] = {
        val target = log2(k) * bandwith
        val rho: DenseVector[Double] = DenseVector.zeros(distances.rows)
        val result: DenseVector[Double] = DenseVector.zeros(distances.rows)

        @annotation.tailrec
        def go(i: Int): Array[DenseVector[Double]] = {
            if (i < distances.rows) {
                var lo: Double = 0d
                var hi = inf
                var mid: Double = 1d

                val ithDistances = distances(i, ::).t
                val distancesGreaterThanZero = ithDistances.findAll(_ > 0)
                val nonZeroDists = ithDistances(distancesGreaterThanZero)

                if (distancesGreaterThanZero.length >= localConnectivity) {
                    val index: Int = floor(localConnectivity).toInt
                    val interpolation: Double = localConnectivity - index

                    if (index > 0) {
                        rho(i) = nonZeroDists(index - 1)
                        if (interpolation > smoothKtolerance)
                            rho(i) = rho(i) + interpolation * (nonZeroDists(index) - nonZeroDists(index - 1))
                    }
                    else {
                        rho(i) = interpolation * nonZeroDists(0)
                    }
                }
                else if (distancesGreaterThanZero.nonEmpty)
                    rho(i) = max(nonZeroDists)

                @annotation.tailrec
                def go2(n: Int): Unit = {
                    if (n < nIter) {
                        var psum = 0d

                        // TODO : make this function with a non-Unit return type
                        def setPsum(j: Int): Unit = {
                            if (j < distances.cols) {
                                val d = distances(i, j) - rho(i)
                                if (d > 0) psum = psum + exp(-(d / mid))
                                else psum = psum + 1d
                                setPsum(j + 1)
                            }
                        }

                        setPsum(1)

                        if (abs(psum - target) >= smoothKtolerance) {
                            if (psum > target) {
                                hi = mid
                                mid = (lo + hi) / 2d
                            }
                            else {
                                lo = mid
                                if (hi == inf) mid = mid * 2
                                else mid = (lo + hi) / 2d
                            }

                            go2(n + 1)
                        }
                    }
                }

                go2(0)

                result(i) = mid


                def meanv(vect: DenseVector[Double]): Double = {
                    sum(vect) / vect.length
                }

                def meanm(mat: DenseMatrix[Double]): Double = {
                    sum(mat) / (mat.cols * mat.rows)
                }

                if (rho(i) > 0D) {
                    if (result(i) < minKdistScale * meanv(ithDistances))
                        result(i) = minKdistScale * meanv(ithDistances)
                }
                else {
                    if (result(i) < minKdistScale * meanm(distances))
                        result(i) = minKdistScale * meanm(distances)
                }

                go(i + 1)
            }
            else Array[DenseVector[Double]](result, rho)
        }

        val res = go(0)
        (res(0), res(1))
    }


    /**
      * Compute the ``nNeighbors`` nearest points for each data point in ``X``
      * under ``dist``. This may be exact, but more likely is approximated via
      * nearest neighbor descent.
      *
      * @param data The input data to compute the k-neighbor graph of.
      * @param nNeighbors The number of nearest neighbors to compute for each sample in ``X``.
      * @param dist The metric to use for the computation.
      * @param angular Whether to use angular rp trees in NN approximation.
      * @return The indices on the ``n_neighbors`` closest points in the dataset.
      *         The distances to the ``n_neighbors`` closest points in the dataset.
      */
    def nearestNeighbors(data: DenseMatrix[Double], nNeighbors: Int, dist: Distance, angular: Boolean = false) : (DenseMatrix[Int], DenseMatrix[Double], Forest)  = {
        val nbTrees = 5 + round(pow(data.rows, 0.5) / 20.0).toInt
        val nbIters = max(5, round(log2(data.rows)).toInt)
        val rngState: Array[Long] = Array(2, 1, 1)

        val metricNNDescent:
            (DenseMatrix[Double], Int, Array[Long], Int, Int, Boolean, Option[DenseMatrix[Int]])
                => (DenseMatrix[Int], DenseMatrix[Double]) =
            (data, nn, rs, mc, ni, rti, leaf)
            => NNDescent.makeNNDescent(dist)(data, nn, rs,
                maxCandidates = mc,
                nIters = ni,
                rpTreeInit = rti,
                leafArray = leaf)

        val rpForest = Forest(data, nNeighbors, nbTrees, rngState, angular)

        val leafArray = rpForest.leafArray

        val (knnIndices, knnDists) = metricNNDescent(
            data,
            nNeighbors,
            rngState,
            60,
            nbIters,
            true,
            Option(leafArray)
        )
        (knnIndices, knnDists, rpForest)
    }

    /**
      * Construct the membership strength data for the 1-skeleton of each local
      * fuzzy simplicial set -- this is formed as a sparse matrix where each row is
      * a local fuzzy simplicial set, with a membership strength for the
      * 1-simplex to each other data point.
      *
      * @param knnIndices The indices on the ``n_neighbors`` closest points in the dataset.
      * @param knnDists   The distances to the ``n_neighbors`` closest points in the dataset.
      * @param sigmas     The normalization factor derived from the metric tensor approximation.
      * @param rhos       The local connectivity adjustment.
      * @return A square matrix that contain the weight between two points.
      */

    def membershipStrengths(knnIndices: DenseMatrix[Int], knnDists: DenseMatrix[Double],
                            sigmas: DenseVector[Double], rhos: DenseVector[Double]): (DenseVector[Int], DenseVector[Int], DenseVector[Double])
    = {
        val nSamples = knnIndices.rows
        val nNeighbors = knnIndices.cols
        val len = nSamples * nNeighbors

        val rows = mutable.ArrayBuffer.fill(len)(0)
        val cols = mutable.ArrayBuffer.fill(len)(0)
        val values = mutable.ArrayBuffer.fill(len)(0D)


        @annotation.tailrec
        def go(i: Int): Unit = {
            if (i < nSamples) {
                var value = 0D

                @annotation.tailrec
                def go2(j: Int): Unit = {

                    def majData : Unit = {
                        rows(i * nNeighbors + j) = i
                        cols(i * nNeighbors + j) = knnIndices(i, j)
                        values(i * nNeighbors + j) = value
                    }

                    if (j < nNeighbors) {
                        if (knnIndices(i, j) == -1)
                            go2(j + 1)
                        else if (knnIndices(i, j) == i) {
                            value = 0D
                            majData
                            go2(j + 1)
                        }
                        else if (knnDists(i, j) - rhos(i) <= 0D) {
                            value = 1D
                            majData
                            go2(j + 1)
                        }
                        else {
                            value = exp(-((knnDists(i, j) - rhos(i)) / sigmas(i)))
                            majData
                            go2(j + 1)
                        }
                    }
                }

                go2(0)
                go(i + 1)
            }
        }
        go(0)
        (DenseVector(rows: _*), DenseVector(cols: _*), DenseVector(values: _*))
    }

    /**
      * Given a set of weights and number of epochs generate the number of
      * epochs per sample for each weight.
      *
      * @param weights The weights of how much we wish to sample each 1-simplex.
      * @param nEpochs The total number of epochs we want to train for.
      * @return An vector of epochs per sample, a number for each 1-simplex.
      */

    def makeEpochsPerSample(weights: mutable.ArrayBuffer[Double], nEpochs: Int) : DenseVector[Double] = {
        val result = -1D * DenseVector.ones[Double](weights.length)
        val nSamples = new DenseVector[Double](weights.length)

        weights.indices.foreach(i => nSamples(i) = nEpochs * weights(i) / max(weights))
        val samplesGreaterThan0 = nSamples.findAll(_ > 0)

        samplesGreaterThan0.foreach(i => result(i) = nEpochs.toDouble / nSamples(i))

        result
    }


    def reduceEuclidean(x: DenseVector[Double], y: DenseVector[Double]): Double = {
        val euclidDists = new DenseVector[Double](x.length)
        (0 until x.length).foreach(i => euclidDists(i) = pow(x(i) - y(i), 2))
        sum(euclidDists)
    }

    /**
      * Improve an embedding using stochastic gradient descent to minimize the
      * fuzzy set cross entropy between the 1-skeletons of the high dimensional
      * and low dimensional fuzzy simplicial sets. In practice this is done by
      * sampling edges based on their membership strength (with the (1-p) terms
      * coming from negative sampling similar to word2vec).
      *
      * @param headEmbedding        The initial embedding to be improved by SGD.
      * @param tailEmbedding        The reference embedding of embedded points. If not embedding new
      *                             previously unseen points with respect to an existing embedding this
      *                             is simply the head_embedding (again); otherwise it provides the
      *                             existing embedding to embed with respect to.
      * @param head                 The indices of the heads of 1-simplices with non-zero membership.
      * @param tail                 The indices of the tails of 1-simplices with non-zero membership.
      * @param nEpochs              The number of training epochs to use in optimization.
      * @param nVertices            The number of vertices (0-simplices) in the dataset.
      * @param epochsPerSample      A float value of the number of epochs per 1-simplex. 1-simplices with
      *                             weaker membership strength will have more epochs between being sampled.
      * @param a                    Parameter of differentiable approximation of right adjoint functor
      * @param b                    Parameter of differentiable approximation of right adjoint functor
      * @param rngState             The internal state of the rng
      * @param gamma                Weight to apply to negative samples.
      * @param initialAlpha         Initial learning rate for the SGD.
      * @param negativeSampleRate   Number of negative samples to use per positive sample.
      * @return The optimized embedding.
      */
    def optimizeLayout(headEmbedding: DenseMatrix[Double], tailEmbedding: DenseMatrix[Double],
                       head: DenseVector[Int], tail: DenseVector[Int],
                       nEpochs: Int, nVertices: Int,
                       epochsPerSample: DenseVector[Double],
                       a: Double, b: Double,
                       rngState: Array[Long],
                       gamma: Double = 1D,
                       initialAlpha: Double = 1D,
                       negativeSampleRate: Int = 5): DenseMatrix[Double] = {
        val dim = headEmbedding.cols
        val moveOther = headEmbedding.rows == tailEmbedding.rows
        var alpha = initialAlpha

        val epochPerNegativeSample = epochsPerSample.map(_ / negativeSampleRate)

        val epochOfNextNegativeSample = epochPerNegativeSample.copy
        val epochOfNextSample = epochsPerSample.copy

        // TODO : make this function with a non-Unit return type
        @annotation.tailrec
        def goOverEpochs(n: Int): Unit = {
            if (n < nEpochs) {
                @annotation.tailrec
                def goOverEPSLength(i: Int): Unit = {
                    if (i < epochsPerSample.length) {
                        if (epochOfNextSample(i) <= n && i != 14) {
                            val j = head(i)
                            var k = tail(i)

                            val current = headEmbedding(j, ::).t
                            var other = tailEmbedding(k, ::).t

                            var distSquared = reduceEuclidean(current, other)
                            var gradCoeff = 0D
                            var gradD = 0D

                            if (distSquared > 0D) {
                                gradCoeff = -2D * a * b * pow(distSquared, b - 1D)
                                gradCoeff = gradCoeff / (a * pow(distSquared, b) + 1D)
                            }

                            def clip(value: Double, clamp: Double): Double = {
                                if (value > clamp) clamp
                                else if (value < -clamp) -clamp
                                else value
                            }

                            for (d <- 0 until dim) {
                                gradD = clip(gradCoeff * (current(d) - other(d)), 4D)
                                current(d) += gradD * alpha
                                if (moveOther) other(d) = other(d) - gradD * alpha
                            }

                            epochOfNextSample(i) += epochsPerSample(i)

                            val nNegSamples = ((n - epochOfNextNegativeSample(i)) / epochPerNegativeSample(i)).toInt

                            @annotation.tailrec
                            def go(p: Int): Unit = {

                                @annotation.tailrec
                                def setCurrent(d: Int): Unit = {
                                    if (d < dim) {
                                        if (gradCoeff > 0D)
                                            gradD = clip(gradCoeff * (current(d) - other(d)), 4D)
                                        else
                                            gradD = 4D
                                        current(d) += gradD * alpha
                                        setCurrent(d + 1)
                                    }
                                }

                                if (p < nNegSamples) {
                                    def mod(a: Int, b: Int) = {
                                        val res = a % b
                                        if (res < 0) res + b else res
                                    }

                                    k = mod(Utils.tauRandInt(rngState), nVertices)
                                    other = tailEmbedding(k, ::).t
                                    distSquared = reduceEuclidean(current, other)

                                    if (distSquared > 0D) {
                                        gradCoeff = 2D * gamma * b
                                        gradCoeff /= (0.001 + distSquared) * (a * pow(distSquared, b) + 1)
                                    }
                                    else {
                                        gradCoeff = 0D
                                    }
                                    setCurrent(0)
                                    go(p + 1)
                                }
                            }

                            go(0)

                            epochOfNextNegativeSample(i) += nNegSamples * epochPerNegativeSample(i)
                        }
                        goOverEPSLength(i + 1)
                    }
                }

                alpha = initialAlpha * (1D - n.toDouble / nEpochs.toDouble)
                goOverEPSLength(0)
                goOverEpochs(n + 1)
            }
        }
        goOverEpochs(0)
        headEmbedding
    }

    /** Perform a fuzzy simplicial set embedding, using a specified
      * initialisation method and then minimizing the fuzzy set cross entropy
      * between the 1-skeletons of the high and low dimensional fuzzy simplicial
      * sets.
      *
      * @param data                 The source data to be embedded by UMAP.
      * @param graph                The 1-skeleton of the high dimensional fuzzy simplicial set as
      *                             represented by a graph for which we require a sparse matrix for the
      *                             (weighted) adjacency matrix.
      * @param nComponents          The dimensionality of the euclidean space into which to embed the data.
      * @param initialAlpha         Initial learning rate for the SGD.
      * @param a                    Parameter of differentiable approximation of right adjoint functor
      * @param b                    Parameter of differentiable approximation of right adjoint functor
      * @param gamma                Weight to apply to negative samples.
      * @param negativeSampleRate   The number of negative samples to select per positive sample
      *                             in the optimization process. Increasing this value will result
      *                             in greater repulsive force being applied, greater optimization
      *                             cost, but slightly more accuracy.
      * @param nEpochs              The number of training epochs to be used in optimizing the
      *                             low dimensional embedding. Larger values result in more accurate
      *                             embeddings. If 0 is specified a value will be selected based on
      *                             the size of the input dataset (200 for large datasets, 500 for small).
      * @param init                 How to initialize the low dimensional embedding. Options are:
      *                             'spectral': use a spectral embedding of the fuzzy 1-skeleton
      *                             'random': assign initial embedding positions at random.
      * @return The optimized of ``graph`` into an ``n_components`` dimensional
      *         euclidean space.
      */

    def simplicialSetEmbedding(data: DenseMatrix[Double], graph: DenseMatrix[Double],
                               nComponents: Int, initialAlpha: Double,
                               a: Double, b: Double, gamma: Double,
                               negativeSampleRate: Int = 5,
                               nEpochs: Int = 0, init: String = "random",
                               initMatrix: Option[DenseMatrix[Double]] = None
                              ): DenseMatrix[Double] = {

        var epochs = nEpochs
        val g = graph
        val nVertices = graph.cols
        implicit val random: Random = new Random

        if (nEpochs <= 0) {
            if (graph.rows <= 10000)
                epochs = 500
            else
                epochs = 200
        }

        val maxGraphOverNEpochs = max(graph) / epochs.toDouble

        // TODO : make this function with a non-Unit return type
        @annotation.tailrec
        def majGraph(i: Int): Unit = {
            if (i < graph.rows) {
                val ind = graph(i, ::).t.findAll(_ < maxGraphOverNEpochs)
                ind.foreach(j => g(i, j) = 0)
                majGraph(i + 1)
            }
        }

        majGraph(0)

        def randomMatrix(implicit random: Random): DenseMatrix[Double] =
            new DenseMatrix[Double](graph.rows, nComponents).map(_ => random.nextDouble * 20D - 10D)

        val embedding = init match {
            case "random" =>
                randomMatrix
            case "spectral" =>
                if (isConnected(graph)) {
                    val initialisation = spectralLayout(data, graph, nComponents)
                    val expansion = 10D / max(abs(initialisation))
                    val normRandom = breeze.stats.distributions.Gaussian(0, 0.0001)
                    val normRandomMatrix = DenseMatrix.rand(graph.rows, nComponents, normRandom)
                    initialisation * expansion + normRandomMatrix
                }
                else {
                    println("The result graph is not connected, so a random matrix is used to initalize the points.")
                    randomMatrix
                }
            case "init" =>
                var matrix = initMatrix.getOrElse(randomMatrix)
                if (initMatrix.isDefined) {
                    val vectorArray = new mutable.ArrayBuffer[Seq[Double]]
                    for (i <- 0 until matrix.rows) {
                        val vector = matrix(i, ::).t.toArray.toSeq
                        if(!vectorArray.contains(vector)) {
                            vectorArray += vector
                        }
                    }
                    val kd = KDTree.fromSeq(vectorArray)(DimensionalOrdering.dimensionalOrderingForSeq[Seq[Double],Double](matrix.cols))
                    val distance = new Euclidean
                    val distArray = Array.fill[Double](matrix.rows)(0D)
                    for (i <- 0 until matrix.rows - 1) {
                        val p = kd.findNearest(matrix(i,::).t.toArray.toSeq, 2)
                        val dist2 = distance(matrix(i,::).t, new DenseVector[Double](p.last.toArray))
                        distArray(i) = dist2
                    }
                    val mean = distArray.foldLeft(0.0){(sum, elem) => sum + (elem / distArray.length)}
                    matrix = DenseMatrix.rand(matrix.rows, matrix.cols, Gaussian(0.0, 0.001 * mean))
                }
                matrix
            case _ =>
                println("No initialization method defines for " + init + ".")
                //throw IllegalArgumentException TODO: catch this exception
                randomMatrix
        }

        val graphArray = graph.toDenseVector(graph.toDenseVector.findAll(_ != 0)).toArray
        val epochsPerSample = makeEpochsPerSample(mutable.ArrayBuffer(graphArray: _*), epochs)
        val gtz = g.findAll(_ > 0)

        val head = new DenseVector[Int](gtz.length)
        val tail = new DenseVector[Int](gtz.length)

        gtz.indices.foreach(i => head(i) = gtz(i)._2)
        gtz.indices.foreach(i => tail(i) = gtz(i)._1)

        val rngState = new Array[Long](3).map(_ => random.nextLong)

         val newEmbedding = optimizeLayout(
            embedding, embedding,
            head, tail,
            epochs, nVertices,
            epochsPerSample,
            a, b,
            rngState,
            gamma, initialAlpha, negativeSampleRate)

        newEmbedding
    }


    def spectralLayout(data: DenseMatrix[Double], graph: DenseMatrix[Double], dim: Int): DenseMatrix[Double] = {
        val L = normalizedLaplaceMatrix(graph)
        val e = eig(L)
        val k = dim + 1
        val (values, vectors) = (e.eigenvalues, e.eigenvectors)

        val sortedEigenValues = DenseVector(argsort(values): _*)
        val kSortedEigenValues = sortedEigenValues(1 until k)
        vectors(::, kSortedEigenValues.toArray.toIndexedSeq).toDenseMatrix
    }

    /**
      * Given an adjency matrix, compute the associated normalised Laplacian matrix
      *
      * @param adjency Adjency matrix
      * @return The Laplacian matrix. Zeros matrix if there are some isolated node(s)
      */
    def normalizedLaplaceMatrix(adjency: DenseMatrix[Double]): DenseMatrix[Double] = {
        val degrees: DenseVector[Double] = sum(adjency(*, ::))

        val dim = adjency.cols

        //Check if there are not isolated nodes
        val zerosVector = DenseVector.zeros[Double](dim)
        var greaterThanZero = new DenseVector[Int](dim)
        (0 until dim).foreach(i => if (degrees(i) < zerosVector(i)) greaterThanZero(i) = 1 else greaterThanZero(i) = 0)
        if (sum(greaterThanZero) > 0) {
            DenseMatrix.zeros[Double](dim, dim)
        }
        else {
            val rootDegrees = sqrt(degrees)
            (0 until dim).foreach(i => rootDegrees(i) = 1 / rootDegrees(i))
            val D = diag(rootDegrees)
            val I = DenseMatrix.eye[Double](adjency.rows)

            I - D * adjency * D
        }
    }

    /**
      * Given an adjency matrix, compute the associated Laplacian matrix
      *
      * @param adjency Adjency matrix
      * @return The Laplacian matrix. Zeros matrix if there are some isolated node(s)
      */
    def laplaceMatrix(adjency: DenseMatrix[Double]): DenseMatrix[Double] = {
        val degrees: DenseVector[Double] = sum(adjency(*, ::))
        val D = diag(degrees)

        D - adjency
    }

    def isConnected(adjency: DenseMatrix[Double]): Boolean = {
        val marking: mutable.ArrayBuffer[Boolean] = mutable.ArrayBuffer.fill(adjency.rows)(false)

        def explore(graph: DenseMatrix[Double], s: Int, marks: mutable.ArrayBuffer[Boolean]): Unit = {
            marks(s) = true
            val sline = graph(s, ::).t
            val child = sline.findAll(i => i > 0.000001 || i < -0.000001)
            if (child.exists(i => !marks(i))) {
                child.foreach(i => if (!marks(i)) explore(graph, i, marks))
            }
        }
        explore(adjency, 0, marking)
        marking.forall(_ == true)
    }

    /**
      * Given a set of data X, a neighborhood size, and a measure of distance
      * compute the fuzzy simplicial set (here represented as a fuzzy graph in
      * the form of a sparse matrix) associated to the data. This is done by
      * locally approximating geodesic distance at each point, creating a fuzzy
      * simplicial set for each such point, and then combining all the local
      * fuzzy simplicial sets into a global one via a fuzzy union.
      *
      * @param X          DenseMatrix (n_samples, n_features)
      *                   The data to be modelled as a fuzzy simplicial set.
      * @param nNeighbors int
      *                   The number of neighbors to use to approximate geodesic distance.
      *                   Larger numbers induce more global estimates of the manifold that can
      *                   miss finer detail, while smaller values will focus on fine manifold
      *                   structure to the detriment of the larger picture.
      * @param metric     function
      *                   The metric to use to compute distances in high dimensional space
      * @param kNNIndices DenseMAtrix (n_samples, n_neighbors) (optional)
      *                   If the k-nearest neighbors of each point has already been calculated
      *                   you can pass them in here to save computation time. This should be
      *                   an array with the indices of the k-nearest neighbors as a row for
      *                   each data point.
      * @param kNNDists   DenseMatrix (n_samples, n_neighbors) (optional)
      *                   If the k-nearest neighbors of each point has already been calculated
      *                   you can pass them in here to save computation time. This should be
      *                   an array with the distances of the k-nearest neighbors as a row for
      *                   each data point.
      * @param angular       bool (optional, default False)
      *                      Whether to use angular/cosine distance for the random projection
      *                      forest for seeding NN-descent to determine approximate nearest
      *                      neighbors.
      * @param setOpMixRatio double (optional, default 1.0)
      *                      Interpolate between (fuzzy) union and intersection as the set operation
      *                      used to combine local fuzzy simplicial sets to obtain a global fuzzy
      *                      simplicial sets. Both fuzzy set operations use the product t-norm.
      *                      The value of this parameter should be between 0.0 and 1.0; a value of
      *         1.0 will use a pure fuzzy union, while 0.0 will use a pure fuzzy
      *                      intersection.
      * @param lc                  double (optional, default 1)
      *                            The local connectivity required -- i.e. the number of nearest
      *                            neighbors that should be assumed to be connected at a local level.
      *                            The higher this value the more connected the manifold becomes
      *           locally. In practice this should be not more than the local intrinsic
      *                            dimension of the manifold.
      * @param verbose             bool (optional, default False)
      *                            Whether to report information on the current progress of the algorithm.
      * @return fuzzySimplicialSet: DenseMatrix
      *         A fuzzy simplicial set represented as a sparse matrix. The (i,
      *         j) entry of the matrix represents the membership strength of the
      *         1-simplex between the ith and jth sample points.
      */

    def fuzzySimplicialSet(X: DenseMatrix[Double], nNeighbors: Int, metric: Distance,
                           kNNIndices: Option[DenseMatrix[Int]] = None, kNNDists: Option[DenseMatrix[Double]] = None,
                           angular: Boolean = false, setOpMixRatio: Double = 1D, lc: Double = 1D, verbose: Boolean = false)
    : DenseMatrix[Double] = {

        val (knni, knnd) = (kNNIndices, kNNDists) match {
            case (None, _) | (_, None) =>
                val nN = nearestNeighbors(X, nNeighbors, metric, angular)
                (nN._1, nN._2)
            case _ => (kNNIndices.get, kNNDists.get)
        }

        val sKNND = smoothKNNDist(knnd, nNeighbors, localConnectivity = lc)

        val sigmas: DenseVector[Double] = sKNND._1
        val rhos: DenseVector[Double] = sKNND._2

        val (rows, cols, vals) = membershipStrengths(knni, knnd, sigmas, rhos)
        val result = Utils.makeMatrix(rows, cols, vals, X.rows, X.rows)

        val transpose = result.t
        val prodMatrix = result *:* transpose
        setOpMixRatio * (result + transpose - prodMatrix) + (1D - setOpMixRatio) * prodMatrix
    }


    /**
      * Fit a, b params for the differentiable curve used in lower
      * dimensional fuzzy simplicial complex construction. We want the
      * smooth curve (from a pre-defined family with simple gradient) that
      * best matches an offset exponential decay.
      *
      * @param spread  The effective scale of embedded points. In combination with ``min_dist``
      *                this determines how clustered/clumped the embedded points are.
      * @param minDist The effective minimum distance between embedded points.
      * @return Tuple of a and b parameters
      */

    def findABParams(spread: Double, minDist: Double): (Double, Double) = {

        /* Vector initialisation */
        var xv: DenseVector[Double] = linspace(0, 3, 300)
        xv(0) = 0.000000001 // To avoid a division by zero
        var yv: DenseVector[Double] = DenseVector.zeros(300)

        val lessThanOne = xv.findAll(x => x < minDist)
        val moreThanOne = xv.findAll(x => x >= minDist)

        lessThanOne.foreach(yv(_) = 1)
        moreThanOne.foreach(i => yv(i) = exp(-(xv(i) - minDist) / 1))

        val wop: Array[WeightedObservedPoint] = new Array[WeightedObservedPoint](xv.length)
        (0 until xv.length).foreach(i => wop(i) = new WeightedObservedPoint(1, xv(i), yv(i)))

        /* The function to fit with his gradient */
        val f: ParametricUnivariateFunction = new ParametricUnivariateFunction {
            override def value(x: Double, parameters: Double*): Double = {
                val a = parameters(0)
                val b = parameters(1)

                1.0 / (1.0 + a * pow(x, 2 * b))
            }

            override def gradient(x: Double, parameters: Double*): Array[Double] = {
                val a = parameters(0)
                val b = parameters(1)
                val powx2b = pow(x, 2 * b)
                val ga = -powx2b / pow(1 + a * powx2b, 2)
                val gb = -(2 * a * powx2b * log(x)) / pow(1 + a * powx2b, 2)
                val grad = Array(ga, gb)
                grad
            }
        }

        val wopl = wop.toList
        val wopc = wopl.asJava

        val cf = SimpleCurveFitter.create(f, Array(1, 1))
        val param = cf.fit(wopc)

        (param(0), param(1))
    }

    def generalSimplicialSetIntersection(simplicialSet1: DenseMatrix[Double], simplicialSet2: DenseMatrix[Double], weight: Double): DenseMatrix[Double] = {
        val result = simplicialSet1 + simplicialSet2

        Sparse.generalSSetIntersection(
            simplicialSet1,
            simplicialSet2,
            result, 
            weight
        )
    }


    /** Under the assumption of categorical distance for the intersecting
      * simplicial set perform a fast intersection.
      *
      * @param simplicialSet : DenseMatrix[Double] The input fuzzy simplicial set
      * @param target : Array[Double] The categorical labels to use in the intersection.
      * @param unknownDist : Double The distance of an unknown label (-1) is assumed to be from any point.
      * @param farDist : Double The distance between unmatched labels.
      */
    def fastIntersection(simplicialSet : DenseMatrix[Double], target : Array[Double], unknownDist : Double = 1D, farDist: Double = 5D): DenseMatrix[Double] = {

        @annotation.tailrec
        def go(nz: Int, na: Int, ss: DenseMatrix[Double]): DenseMatrix[Double] = {
            if (nz < ss.rows) {
                if (na < ss.cols) {
                    if (target(nz) == -1 || target(na) == -1)
                        ss(nz, na) *:*= exp(-unknownDist)
                    else if (target(na) != target(nz))
                        ss(na, nz) *:*= exp(-farDist)
                    go(nz, na + 1, ss)
                } else go(nz + 1, 0, ss)
            } else ss
        }

        go(0, 0, simplicialSet)
    }
    
    /**
      * Reset the local connectivity requirement -- each data sample should
      * have complete confidence in at least one 1-simplex in the simplicial set.
      * We can enforce this by locally rescaling confidences, and then remerging the
      * different local simplicial sets together.
      *
      * @param simplicialSet The simplicial set for which to recalculate with respect to local
      *                      connectivity.
      * @return The recalculated simplicial set, now with the local connectivity
      *         assumption restored.
      */
    def resetLocalConnectivity(simplicialSet: DenseMatrix[Double]): DenseMatrix[Double] = {

        @annotation.tailrec
        def go(i: Int, normalizedSimpSet: DenseMatrix[Double]): DenseMatrix[Double] = {
            if (i < simplicialSet.rows) {
                normalizedSimpSet(i, ::) := simplicialSet(i, ::) / max(simplicialSet(i, ::))
                go(i + 1, normalizedSimpSet)
            } else normalizedSimpSet
        }

        val normalizedSimpSet = go(0, simplicialSet)
        val transpose = normalizedSimpSet.t
        val prod = normalizedSimpSet *:* transpose

        normalizedSimpSet + transpose - prod
    }

    /**Combine a fuzzy simplicial set with another fuzzy simplicial set generated from categorical data
      * using categorical distances. The target data is assumed to be categorical label data (a vector
      * of labels), and this will update the fuzzy simplicial set to respect that label data.
      *
      * @param simplicialSet : DenseMatrix[Double] The input fuzzy simplicial set
      * @param target : Array[Double] of shape nSamples The categorical labels to use in the intersection
      * @param unknownDist : Double (optional, default 1D) The distance of an unknown
      *                      label (-1) is assumed to be from any point
      * @param farDist     : Double (optional, Default 5D) The distance between unmatched labels
      * @return simplicialSet : DenseMatrix[Double] The resulting intersected fuzzy simplicial set
      */
    def categoricalSimplicialSetIntersection(simplicialSet : DenseMatrix[Double], target : Array[Double], unknownDist : Double = 1D, farDist: Double = 5D): DenseMatrix[Double] = {

        val ss = fastIntersection(simplicialSet,target,unknownDist,farDist)
        resetLocalConnectivity(ss)
    }


    def initTransform(indices: DenseMatrix[Int], weights: DenseMatrix[Double], embedding: DenseMatrix[Double]): DenseMatrix[Double] = {
        val result = DenseMatrix.zeros[Double](indices.rows, embedding.cols)

        for (i <- 0 until indices.rows)
            for(j <- 0 until indices.cols)
                for (d <- 0 until embedding.cols)
                    result(i, d) += weights(i, j) * embedding(indices(i, j), d)
        result
    }

}


/**
  * Uniform Manifold Approximation and Projection
  *
  * Finds a low dimensional embedding of the data that approximates
  * an underlying manifold.
  *
  * @param nNeighbors  double (optional, default 15)
  *                    The size of local neighborhood (in terms of number of neighboring
  *                    sample points) used for manifold approximation. Larger values
  *                    result in more global views of the manifold, while smaller
  *                    values result in more local data being preserved. In general
  *                    values should be in the range 2 to 100.
  *
  * @param nComponents int (optional, default 2)
  *                    The dimension of the space to embed into. This defaults to 2 to
  *                    provide easy visualization, but can reasonably be set to any
  *                    integer value in the range 2 to 100.
  *
  * @param metric      function (optional, default 'euclidean')
  *                    The metric to use to compute distances in high dimensional space.
  *
  * @param nEpochs     int (optional, default None)
  *                    The number of training epochs to be used in optimizing the
  *                    low dimensional embedding. Larger values result in more accurate
  *         embeddings. If None is specified a value will be selected based on
  *                    the size of the input dataset (200 for large datasets, 500 for small).
  *
  * @param learningRate double (optional, default 1.0)
  *                     The initial learning rate for the embedding optimization.
  *
  * @param init         string (optional, default 'random')
  *                     How to initialize the low dimensional embedding. Options are:
  *                     * 'spectral': use a spectral embedding of the fuzzy 1-skeleton
  *                     * 'random': assign initial embedding positions at random.
  *                     * A numpy array of initial embedding positions.
  *
  * @param minDist      double (optional, default 0.1)
  *                     The effective minimum distance between embedded points. Smaller values
  *                     will result in a more clustered/clumped embedding where nearby points
  *                     on the manifold are drawn closer together, while larger values will
  *                     result on a more even dispersal of points. The value should be set
  *                     relative to the ``spread`` value, which determines the scale at which
  *                     embedded points will be spread out.
  *
  * @param spread        double (optional, default 1.0)
  *                      The effective scale of embedded points. In combination with ``min_dist``
  *                      this determines how clustered/clumped the embedded points are.
  *
  * @param setOPMixRatio double (optional, default 1.0)
  *                      Interpolate between (fuzzy) union and intersection as the set operation
  *                      used to combine local fuzzy simplicial sets to obtain a global fuzzy
  *                      simplicial sets. Both fuzzy set operations use the product t-norm.
  *                      The value of this parameter should be between 0.0 and 1.0; a value of
  *         1.0 will use a pure fuzzy union, while 0.0 will use a pure fuzzy
  *                      intersection.
  *
  * @param localConnectivity int (optional, default 1)
  *                          The local connectivity required -- i.e. the number of nearest
  *                          neighbors that should be assumed to be connected at a local level.
  *                          The higher this value the more connected the manifold becomes
  *         locally. In practice this should be not more than the local intrinsic
  *                          dimension of the manifold.
  *
  * @param repulsionStrength double (optional, default 1.0)
  *                          Weighting applied to negative samples in low dimensional embedding
  *         optimization. Values higher than one will result in greater weight
  *                          being given to negative samples.
  *
  * @param negativeSampleRate int (optional, default 5)
  *                           The number of negative samples to select per positive sample
  *                           in the optimization process. Increasing this value will result
  *                           in greater repulsive force being applied, greater optimization
  *                           cost, but slightly more accuracy.
  *
  * @param transformQueueSize double (optional, default 4.0)
  *                           For transform operations (embedding new points using a trained model_
  *                           this will control how aggressively to search for nearest neighbors.
  *                           Larger values will result in slower performance but more accurate
  *                           nearest neighbor evaluation.
  *
  * @param a                  double (optional, default None)
  *                           More specific parameters controlling the embedding. If None these
  *                           values are set automatically as determined by ``min_dist`` and
  *                           ``spread``.
  *
  * @param b                  double (optional, default None)
  *                           More specific parameters controlling the embedding. If None these
  *                           values are set automatically as determined by ``min_dist`` and
  *                           ``spread``.
  *
  * @param angularRPForest bool (optional, default False)
  *                        Whether to use an angular random projection forest to initialise
  *                        the approximate nearest neighbor search. This can be faster, but is
  *                        mostly on useful for metric that use an angular style distance such
  *                        as cosine, correlation etc. In the case of those metrics angular forests
  *                        will be chosen automatically.
  *
  * @param transformSeed   int (optional, default 42)
  *                        Random seed used for the stochastic aspects of the transform operation.
  *                        This ensures consistency in transform operations.
  *
  * @param verbose         bool (optional, default False)
  *                        Controls verbosity of logging.
  */
class UMAP(var nNeighbors: Int = 15,
           val nComponents: Int = 2,
           val metric: Distance = new Euclidean,
           val nEpochs: Option[Int] = None,
           val learningRate: Double = 1.0,
           val init: String = "random",
           val initMatrix: Option[DenseMatrix[Double]] = None,
           val minDist: Double = 0.1,
           val spread: Double = 1.0,
           val setOPMixRatio: Double = 1.0,
           val localConnectivity: Double = 1.0,
           val repulsionStrength: Double = 1.0,
           val negativeSampleRate: Int = 5,
           val transformQueueSize: Double = 4.0,
           var a: Option[Double] = None,
           var b: Option[Double] = None,
           val angularRPForest: Boolean = false,
           val targetNNeighbors: Int = -1,
           val targetMetric: Option[Distance] = None,
           val targetWeight: Double = 0.5,
           val transformSeed: Int = 42,
           val verbose: Boolean = false
            ) {

    private var knnIndices: Option[DenseMatrix[Int]] = None
    private var knnDists: Option[DenseMatrix[Double]] = None
    private var rpForest: Option[Forest] = None
    private var graph: Option[DenseMatrix[Double]] = None
    private var searchGraph: Option[DenseMatrix[Int]] = None
    private var embedding: Option[DenseMatrix[Double]] = None
    private var data: Option[DenseMatrix[Double]] = None
    private val initialAlpha = learningRate


    def fit(X: DenseMatrix[Double], y: Option[DenseVector[Double]] = None): DenseMatrix[Double] = {
        var oneRow = false
        val (ares, bres) = UMAP.findABParams(spread, minDist)


        if (X.rows <= nNeighbors) {
            if (X.rows == 1)
                oneRow = true
            nNeighbors = X.rows - 1
        }

        if (!oneRow) {

            val (knni, knnd, rpf) = UMAP.nearestNeighbors(X, nNeighbors, metric, angularRPForest)
            val g: DenseMatrix[Double] = UMAP.fuzzySimplicialSet(X, nNeighbors, metric,
                Some(knni), Some(knnd),
                angularRPForest, setOPMixRatio, localConnectivity)

            /* Search Graph */
            val sgAux = new DenseMatrix[Int](X.rows, X.rows)
            for (i <- 0 until knni.rows)
                for (j <- 0 until knni.cols)
                    sgAux(i, knni(i, j)) = knnd(i, j) match {
                        case 0D => 0
                        case _ => 1
                    }

            val lessThanTranspose: DenseMatrix[Boolean] = sgAux <:< sgAux.t
            val sg = sgAux
            val valuesToMAJ = lessThanTranspose.findAll(a => a)
            valuesToMAJ.foreach(key => sg(key) = sgAux(key))

            if (y.isDefined) {
                // TODO : Exception a lever si jamais la taille des deux matrices n'est pas la meme
                val tm = targetMetric.getOrElse("categorical")
                tm match {
                    case "categorical" =>
                        val fd = targetWeight match {
                            case x if x < 1D =>
                                2.5 * (1D / (1D - targetWeight))
                            case _ => 1e12D
                        }
                        graph = Some(UMAP.categoricalSimplicialSetIntersection(g, y.get.toArray, farDist = fd))
                    case _ =>
                        val tnn = targetNNeighbors match {
                            case -1 => nNeighbors
                            case _ => targetNNeighbors
                        }
                        val targetGraph = UMAP.fuzzySimplicialSet(y.get.toDenseMatrix, tnn, targetMetric.getOrElse(new Euclidean), None, None, false, 1D, 1D, false)

                        graph = Some(UMAP.generalSimplicialSetIntersection(g, targetGraph, targetWeight))
                        graph = Some(UMAP.resetLocalConnectivity(g))
                }
            }


            val ne = nEpochs.getOrElse(0)

            data = Some(X)
            knnIndices = Some(knni)
            knnDists = Some(knnd)
            rpForest = Some(rpf)
            if (graph.isEmpty) graph = Some(g)
            searchGraph = Some(sg)
            a = Some(ares)
            b = Some(bres)
            embedding = Some(UMAP.simplicialSetEmbedding(X, g, nComponents, learningRate, ares, bres, repulsionStrength, negativeSampleRate, ne, init, initMatrix))
            embedding.get
        }
        else {
            DenseMatrix.zeros[Double](1, nComponents)
        }
    }

    def transform (X: DenseMatrix[Double]): DenseMatrix[Double] = {
        if (embedding.get.rows <= 1)
            throw new IllegalArgumentException("Transform unavailable with only a single data sample")

        val rand = new Random(transformSeed)
        val rangeState : Array[Long] = Array.fill(3) {rand.nextLong()}

        var init = NNDescent.initialiseSearch(rpForest.get, data.get, X, (transformQueueSize  * nNeighbors).toInt, rangeState, metric)
        val search = NNDescent.makeInitialisedNNDSearch(metric) _
        val res = search(data.get, init, X)

        val (allindices, alldists) = res.deheapSort
        val indices = allindices(::, 0 until nNeighbors)
        val dists = alldists(::, 0 until nNeighbors)
        val adjustedLocalConnectivity = max(0D, localConnectivity - 1D)
        val smooth = UMAP.smoothKNNDist(dists, nNeighbors, localConnectivity = adjustedLocalConnectivity)

        val (rows, cols, vals) = UMAP.membershipStrengths(indices, dists, smooth._1, smooth._2)
        val graph = Utils.makeMatrix(rows, cols, vals, X.rows, data.get.rows)


        val normalizedGraph = normalize(graph, Axis._1, 1)
        val wei = DenseVector((for (i <- 0 until rows.length) yield normalizedGraph(rows(i), cols(i))): _*)
        val inds = cols.toDenseMatrix.reshape(nNeighbors, X.rows).t
        val weights = wei.toDenseMatrix.reshape(nNeighbors, X.rows).t

        val emb = UMAP.initTransform(indices, weights, embedding.get)
        val ne = nEpochs match {
            case None => if (graph.rows <= 10000) 100 else 30
            case _ => nEpochs.get / 3
        }

        val limit = max(graph) / ne.toDouble
        val lessThanLimit = graph.findAll(n => n < limit && n != 0)
        lessThanLimit.foreach(key => graph(key) = 0D)
        val nzv = Sparse.nonZeroValues(graph)
        val nzr = Sparse.nonZeroRows(graph)
        val nzc = Sparse.nonZeroCols(graph)
        val epochsPerSample = UMAP.makeEpochsPerSample(nzv, ne)

        // TODO : les gerer autrement
        val head = DenseVector(nzr.toArray: _*)
        val tail = DenseVector(nzc.toArray: _*)

        UMAP.optimizeLayout(
            emb, embedding.get,
            head, tail,
            ne, graph.cols,
            epochsPerSample,
            a.get, b.get,
            Array(2, 1, 1),
            repulsionStrength, initialAlpha, negativeSampleRate
        )
    }

}
