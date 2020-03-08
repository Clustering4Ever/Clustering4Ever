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

import _root_.scala.collection.JavaConverters._
import _root_.scala.collection.mutable
import _root_.scala.collection.parallel.mutable.ParArray
import _root_.scala.util.Random

object UMAP {

  private[clustering4ever] val smoothKtolerance = 0.00001
  private[clustering4ever] val minKdistScale = 0.001

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
  private[clustering4ever] def smoothKNNDist(
    distances: DenseMatrix[Double],
    k: Int,
    nIter: Int = 64,
    localConnectivity: Double = 1.0,
    bandwith: Double = 1.0
  ): (DenseVector[Double], DenseVector[Double]) = {

    val target = log2(k) * bandwith
    val rho: DenseVector[Double] = DenseVector.zeros(distances.rows)
    val result: DenseVector[Double] = DenseVector.zeros(distances.rows)

    @annotation.tailrec
    def go(i: Int): Unit = {

      @annotation.tailrec
      def go2(n: Int, lo: Double, hi: Double, mid: Double): Double = {
        
        @annotation.tailrec
        def goPSum(j: Int, pSum: Double): Double = {
          if (j < distances.cols) {
            val d = distances(i, j) - rho(i)
            if (d > 0) goPSum(j + 1, pSum + exp(-(d / mid)))
            else goPSum(j + 1, pSum + 1D)
          }
          else pSum
        }

        if (n < nIter) {
          val pSum = goPSum(1, 0D)
          if (abs(pSum - target) >= smoothKtolerance) {
            if (pSum > target) go2(n + 1, lo, mid, (lo + mid) / 2D)
            else {
              if (hi == inf) go2(n + 1, mid, hi, mid * 2)
              else go2(n + 1, mid, hi, (lo + hi) / 2D)
            }
          }
          else mid
        }
        else mid
      }

      def meanv(vect: DenseVector[Double]): Double = {
          sum(vect) / vect.length
      }

      def meanm(mat: DenseMatrix[Double]): Double = {
          sum(mat) / (mat.cols * mat.rows)
      }

      if (i < distances.rows) {

          val ithDistances = distances(i, ::).t
          val distancesGreaterThanZero = ithDistances.findAll(_ > 0)
          val nonZeroDists = ithDistances(distancesGreaterThanZero)

          if (distancesGreaterThanZero.length >= localConnectivity) {

              val index: Int = floor(localConnectivity).toInt
              val interpolation: Double = localConnectivity - index

              if (index > 0) {
                rho(i) = nonZeroDists(index - 1)
                if (interpolation > smoothKtolerance) {
                  rho(i) = rho(i) + interpolation * (nonZeroDists(index) - nonZeroDists(index - 1))
                }
              }
              else {
                rho(i) = interpolation * nonZeroDists(0)
              }

          }
          else if (distancesGreaterThanZero.nonEmpty) {
            rho(i) = max(nonZeroDists)
          }
          else Unit

          result(i) = go2(0, 0D, inf, 1D)

          if (rho(i) > 0D) {
              if (result(i) < minKdistScale * meanv(ithDistances)) {
                result(i) = minKdistScale * meanv(ithDistances)
              }
          }
          else {
              if (result(i) < minKdistScale * meanm(distances)) {
                result(i) = minKdistScale * meanm(distances)
              }
          }

          go(i + 1)
      }
      else Unit
    }

    go(0)
    (result, rho)
  }


  /**
    * Compute the ``nNeighbors`` nearest points for each data point in ``xData``
    * under ``dist``. This may be exact, but more likely is approximated via
    * nearest neighbor descent.
    *
    * @param data The input data to compute the k-neighbor graph of.
    * @param nNeighbors The number of nearest neighbors to compute for each sample in ``xData``.
    * @param dist The metric to use for the computation.
    * @param angular Whether to use angular rp trees in NN approximation.
    * @return The indices on the ``n_neighbors`` closest points in the dataset.
    *         The distances to the ``n_neighbors`` closest points in the dataset.
    */
  private[clustering4ever] def nearestNeighbors(data: DenseMatrix[Double], nNeighbors: Int, dist: Distance, angular: Boolean = false): (DenseMatrix[Int], DenseMatrix[Double], Forest)  = {
    val nbTrees = 5 + round(pow(data.rows, 0.5) / 20.0).toInt
    val nbIters = max(5, round(log2(data.rows)).toInt)
    val rngState: Array[Long] = Array(2, 1, 1)

    val metricNNDescent: (DenseMatrix[Double], Int, Array[Long], Int, Int, Boolean, Option[DenseMatrix[Int]]) => (DenseMatrix[Int], DenseMatrix[Double]) = (data, nn, rs, mc, ni, rti, leaf) => NNDescent.makeNNDescent(dist)(data, nn, rs, maxCandidates = mc, nIters = ni, rpTreeInit = rti, leafArray = leaf)

    val rpForest = Forest(data, nNeighbors, nbTrees, rngState, angular)

    val leafArray = rpForest.leafArray

    val (knnIndices, knnDists) = metricNNDescent(data, nNeighbors, rngState, 60, nbIters, true, Option(leafArray))
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
  private[clustering4ever] def membershipStrengths(knnIndices: DenseMatrix[Int], knnDists: DenseMatrix[Double], sigmas: DenseVector[Double], rhos: DenseVector[Double]): (DenseVector[Int], DenseVector[Int], DenseVector[Double]) = {
    val nSamples = knnIndices.rows
    val nNeighbors = knnIndices.cols
    val len = nSamples * nNeighbors

    val rows = Array.fill(len)(0)
    val cols = Array.fill(len)(0)
    val values = Array.fill(len)(0D)


    def majData(i: Int, j: Int, value: Double): Unit = {
        rows(i * nNeighbors + j) = i
        cols(i * nNeighbors + j) = knnIndices(i, j)
        values(i * nNeighbors + j) = value
    }

    @annotation.tailrec
    def goI(i: Int): Unit = {

        if (i < nSamples) {
            var value = 0D
            @annotation.tailrec
            def goJ(j: Int): Unit = {
                if (j < nNeighbors) {
                    if (knnIndices(i, j) == -1) {
                      goJ(j + 1)
                    }
                    else if (knnIndices(i, j) == i) {
                        value = 0D
                        majData(i, j, value)
                        goJ(j + 1)
                    }
                    else if (knnDists(i, j) - rhos(i) <= 0D) {
                        value = 1D
                        majData(i, j, value)
                        goJ(j + 1)
                    }
                    else {
                        value = exp(-((knnDists(i, j) - rhos(i)) / sigmas(i)))
                        majData(i, j, value)
                        goJ(j + 1)
                    }
                }
                else Unit
            }
            goJ(0)
            goI(i + 1)
        }
        else Unit
    }
    goI(0)
    (DenseVector(rows), DenseVector(cols), DenseVector(values))
  }

  /**
    * Given a set of weights and number of epochs generate the number of
    * epochs per sample for each weight.
    *
    * @param weights The weights of how much we wish to sample each 1-simplex.
    * @param nEpochs The total number of epochs we want to train for.
    * @return An vector of epochs per sample, a number for each 1-simplex.
    */

  private[clustering4ever] def makeEpochsPerSample(weights: Array[Double], nEpochs: Int): DenseVector[Double] = {
    val result = -1D * DenseVector.ones[Double](weights.length)
    val nSamples = new DenseVector[Double](weights.length)

    weights.indices.foreach(i => nSamples(i) = nEpochs * weights(i) / max(weights))
    val samplesGreaterThan0 = nSamples.findAll(_ > 0)

    samplesGreaterThan0.foreach(i => result(i) = nEpochs.toDouble / nSamples(i))

    result
  }


  private[clustering4ever] def reduceEuclidean(x: DenseVector[Double], y: DenseVector[Double]): Double = {
    var i = 0
    var sum = 0D
    while (i < x.length) {
      val toPow2 = x(i) - y(i)
      sum += toPow2 * toPow2
      i +=1
    }
    sum
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
  private[clustering4ever] def optimizeLayout(
    headEmbedding: DenseMatrix[Double],
    tailEmbedding: DenseMatrix[Double],
    head: DenseVector[Int],
    tail: DenseVector[Int],
    nEpochs: Int,
    nVertices: Int,
    epochsPerSample: DenseVector[Double],
    a: Double,
    b: Double,
    rngState: Array[Long],
    gamma: Double = 1D,
    initialAlpha: Double = 1D,
    negativeSampleRate: Int = 5
  ): DenseMatrix[Double] = {

    val dim = headEmbedding.cols
    val moveOther = headEmbedding.rows == tailEmbedding.rows
    var alpha = initialAlpha

    val epochPerNegativeSample = epochsPerSample.map(_ / negativeSampleRate)


    val epochOfNextSample = epochsPerSample.copy
    val epochOfNextNegativeSample = epochPerNegativeSample.copy


    def clip(value: Double, clamp: Double): Double = {
        if (value > clamp) clamp
        else if (value < -clamp) -clamp
        else value
    }
    def mod(a: Int, b: Int) = {
      val res = a % b
      if (res < 0) res + b else res
    }

    val cst1 = -2D * a * b
    val cst2 = 2D * gamma * b

    def fillEpochSamples(n: Int, i: Int, alpha: Double): Unit = {

      if (epochOfNextSample(i) <= n && i != 14) {
        val j = head(i)
        val k = tail(i)
        val current: DenseVector[Double] = headEmbedding(j, ::).t
        val other: DenseVector[Double] = tailEmbedding(k, ::).t
        val distSquared0 = reduceEuclidean(current, other)

        val gradCoeff0 = if (distSquared0 > 0D) (cst1 * pow(distSquared0, b - 1D)) / (a * pow(distSquared0, b) + 1D) else 0D

        @annotation.tailrec
        def goGradD(d: Int, gradD1: Double): Double = {
          if (d < dim) {
            val gradDOut = clip(gradCoeff0 * (current(d) - other(d)), 4D)
            current(d) += gradDOut * alpha
            if (moveOther) {
              other(d) = other(d) - gradDOut * alpha
            }
            goGradD(d + 1, gradDOut)
          }
          else gradD1
        }

        val gradD = goGradD(0, 0D)

        epochOfNextSample(i) += epochsPerSample(i)

        val nNegSamples = ((n - epochOfNextNegativeSample(i)) / epochPerNegativeSample(i)).toInt

        @annotation.tailrec
        def setCurrent(d: Int, gradCoeff3: Double, other3: DenseVector[Double]): Unit = {
          if (d < dim) {
              val gradD2 = if (gradCoeff3 > 0D) clip(gradCoeff3 * (current(d) - other3(d)), 4D) else 4D
              current(d) += gradD2 * alpha
              setCurrent(d + 1, gradCoeff3, other3)
          }
          else Unit
        }

        def fillK = mod(Utils.tauRandInt(rngState), nVertices)
        val kIndices = Array.fill(nNegSamples)(fillK)
        val otherOuts = kIndices.map(tailEmbedding(_, ::).t)
        val distSquareds = otherOuts.map(reduceEuclidean(current, _))
        val gradCoeffs = distSquareds.map{ distSquar =>
          if (distSquar > 0D) cst2 / ((0.001 + distSquar) * (a * pow(distSquar, b) + 1)) else 0D
        }

        gradCoeffs.zip(otherOuts).foreach{ case (gradCoeff, otherOut) => setCurrent(0, gradCoeff, otherOut)}
        epochOfNextNegativeSample(i) += nNegSamples * epochPerNegativeSample(i)
        gradCoeffs.last
      }

    }

    @annotation.tailrec
    def goOverEpochsTR(n: Int): Unit = {
      if (n < nEpochs) {
        val alphaIn = initialAlpha * (1D - n.toDouble / nEpochs.toDouble)
        (0 until epochsPerSample.length).par.foreach( i => fillEpochSamples(n, i, alphaIn) )
        goOverEpochsTR(n + 1)
      }
      else Unit
    }

    goOverEpochsTR(0)
    
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

  private[clustering4ever] def simplicialSetEmbedding(graph: DenseMatrix[Double], nComponents: Int, initialAlpha: Double, a: Double, b: Double, gamma: Double, negativeSampleRate: Int = 5, nEpochs: Int = 0, init: UMAPInitialization = RandomInit): DenseMatrix[Double] = {

    val g = graph
    val nVertices = graph.cols
    val random: Random = new Random

    val epochs = if (nEpochs <= 0) if (graph.rows <= 10000) 500 else 200 else nEpochs
    val maxGraphOverNEpochs = max(graph) / epochs.toDouble

    @annotation.tailrec
    def majGraph(i: Int): Unit = {
      if (i < graph.rows) {
        val ind = graph(i, ::).t.findAll(_ < maxGraphOverNEpochs)
        ind.foreach(j => g(i, j) = 0)
        majGraph(i + 1)
      }
      else Unit
    }

    majGraph(0)

    def randomMatrix(random: Random): DenseMatrix[Double] = {
      def rdm = random.nextDouble * 20D - 10D
      DenseMatrix.fill[Double](graph.rows, nComponents)(rdm)
    }

    val embedding = init match {
        case RandomInit => randomMatrix(random)
        case SpectralInit => {
          if (isConnected(graph)) {
              val initialisation = spectralLayout(graph, nComponents)
              val expansion = 10D / max(abs(initialisation))
              val normRandom = breeze.stats.distributions.Gaussian(0, 0.0001)
              val normRandomMatrix = DenseMatrix.rand(graph.rows, nComponents, normRandom)
              initialisation * expansion + normRandomMatrix
          }
          else {
              println("The result graph is not connected, so a random matrix is used to initalize the points.")
              randomMatrix(random)
          }
        }
        case CustomInit(matrix) => {              
          val vectorArray = new mutable.ArrayBuffer[Seq[Double]]
          (0 until matrix.rows).foreach{ i =>
              val vector = matrix(i, ::).t.toArray.toSeq
              if (!vectorArray.contains(vector)) {
                  vectorArray += vector
              }
          }
          val kd = KDTree.fromSeq(vectorArray)(DimensionalOrdering.dimensionalOrderingForSeq[Seq[Double], Double](matrix.cols))
          val distance = new Euclidean
          val distArray = Array.fill[Double](matrix.rows)(0D)
          (0 until matrix.rows - 1).foreach { i =>
              val p = kd.findNearest(matrix(i,::).t.toArray.toSeq, 2)
              val dist2 = distance(matrix(i,::).t, new DenseVector[Double](p.last.toArray))
              distArray(i) = dist2
          }
          val mean = distArray.foldLeft(0D){ (sum, elem) => sum + (elem / distArray.length) }
          DenseMatrix.rand(matrix.rows, matrix.cols, Gaussian(0.0, 0.001 * mean))
        }
    }

    val graphArray = graph.toDenseVector(graph.toDenseVector.findAll(_ != 0)).toArray
    val epochsPerSample = makeEpochsPerSample(graphArray, epochs)
    val gtz = g.findAll(_ > 0)

    val head = new DenseVector[Int](gtz.length)
    val tail = new DenseVector[Int](gtz.length)

    gtz.indices.foreach(i => head(i) = gtz(i)._2)
    gtz.indices.foreach(i => tail(i) = gtz(i)._1)

    def rdmFill = random.nextLong
    val rngState = Array.fill[Long](3)(rdmFill)

    val newEmbedding = optimizeLayout(embedding, embedding, head, tail, epochs, nVertices, epochsPerSample, a, b, rngState, gamma, initialAlpha, negativeSampleRate)

    newEmbedding
  }


  private[clustering4ever] def spectralLayout(graph: DenseMatrix[Double], dim: Int): DenseMatrix[Double] = {
    val l = normalizedLaplaceMatrix(graph)
    val e = eig(l)
    val k = dim + 1
    val (values, vectors) = (e.eigenvalues, e.eigenvectors)
    val sortedEigenValues = DenseVector(argsort(values): _*)
    val kSortedEigenValues = sortedEigenValues(1 until k)
    vectors(::, kSortedEigenValues.toArray.toIndexedSeq).toDenseMatrix
  }

  /**
    * Given an adjency matrix, compute the associated normalised Laplacian matrix
    *f
    * @param adjency Adjency matrix
    * @return The Laplacian matrix. Zeros matrix if there are some isolated node(s)
    */
  private[clustering4ever] def normalizedLaplaceMatrix(adjency: DenseMatrix[Double]): DenseMatrix[Double] = {
    val degrees: DenseVector[Double] = sum(adjency(*, ::))
    val dim = adjency.cols
    //Check if there are not isolated nodes
    val zerosVector = DenseVector.zeros[Double](dim)
    val greaterThanZero = new DenseVector[Int](dim)
    (0 until dim).foreach(i => if (degrees(i) < zerosVector(i)) greaterThanZero(i) = 1 else greaterThanZero(i) = 0)
    if (sum(greaterThanZero) > 0) DenseMatrix.zeros[Double](dim, dim)
    else {
        val rootDegrees = sqrt(degrees)
        (0 until dim).foreach(i => rootDegrees(i) = 1 / rootDegrees(i))
        val d = diag(rootDegrees)
        val i = DenseMatrix.eye[Double](adjency.rows)
        i - d * adjency * d
    }
  }
  /**
   * Given an adjency matrix, compute the associated Laplacian matrix
   *
   * @param adjency Adjency matrix
   * @return The Laplacian matrix. Zeros matrix if there are some isolated node(s)
   */
  private[clustering4ever] def laplaceMatrix(adjency: DenseMatrix[Double]): DenseMatrix[Double] = {
    val degrees: DenseVector[Double] = sum(adjency(*, ::))
    val d = diag(degrees)
    d - adjency
  }
  /**
   *
   */
  private[clustering4ever] def isConnected(adjency: DenseMatrix[Double]): Boolean = {

    val marking: Array[Boolean] = Array.fill(adjency.rows)(false)

    def explore(graph: DenseMatrix[Double], s: Int, marks: Array[Boolean]): Unit = {
        marks(s) = true
        val sline = graph(s, ::).t
        val childIn: IndexedSeq[Int] = sline.findAll( i => i > 0.000001 || i < -0.000001 )
        if (childIn.exists(!marks(_))) {
          childIn.foreach( i => if (!marks(i)) explore(graph, i, marks) )
        }
        else Unit
    }

    explore(adjency, 0, marking)
    !marking.exists(!_)

  }
  /**
    * Given a set of data xData, a neighborhood size, and a measure of distance
    * compute the fuzzy simplicial set (here represented as a fuzzy graph in
    * the form of a sparse matrix) associated to the data. This is done by
    * locally approximating geodesic distance at each point, creating a fuzzy
    * simplicial set for each such point, and then combining all the local
    * fuzzy simplicial sets into a global one via a fuzzy union.
    *
    * @param xData          DenseMatrix (n_samples, n_features)
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
  private[clustering4ever] def fuzzySimplicialSet(xData: DenseMatrix[Double], nNeighbors: Int, metric: Distance, kNNIndices: Option[DenseMatrix[Int]] = None, kNNDists: Option[DenseMatrix[Double]] = None, angular: Boolean = false, setOpMixRatio: Double = 1D, lc: Double = 1D, verbose: Boolean = false): DenseMatrix[Double] = {

      val (knni, knnd) = (kNNIndices, kNNDists) match {
        case (None, _) | (_, None) =>
            val nN = nearestNeighbors(xData, nNeighbors, metric, angular)
            (nN._1, nN._2)
        case _ => (kNNIndices.get, kNNDists.get)
      }

      val sKNND = smoothKNNDist(knnd, nNeighbors, localConnectivity = lc)

      val sigmas: DenseVector[Double] = sKNND._1
      val rhos: DenseVector[Double] = sKNND._2

      val (rows, cols, vals) = membershipStrengths(knni, knnd, sigmas, rhos)
      val result = Utils.makeMatrix(rows, cols, vals, xData.rows, xData.rows)

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
  private[clustering4ever] def findABParams(spread: Double, minDist: Double): (Double, Double) = {

      /* Vector initialisation */
      val xv: DenseVector[Double] = linspace(0, 3, 300)
      xv(0) = 0.000000001 // To avoid a division by zero
      val yv: DenseVector[Double] = DenseVector.zeros(300)

      val lessThanOne = xv.findAll(_ < minDist)
      val moreThanOne = xv.findAll(_ >= minDist)

      lessThanOne.foreach(yv(_) = 1)
      moreThanOne.foreach(i => yv(i) = exp(-(xv(i) - minDist) / 1))

      val wop: Array[WeightedObservedPoint] = new Array[WeightedObservedPoint](xv.length)
      (0 until xv.length).foreach(i => wop(i) = new WeightedObservedPoint(1, xv(i), yv(i)))

      /* The function to fit with his gradient */
      val f: ParametricUnivariateFunction = new ParametricUnivariateFunction {
          override def value(x: Double, parameters: Double*): Double = {
              val a = parameters(0)
              val b = parameters(1)
              1D / (1D + a * pow(x, 2 * b))
          }

          override def gradient(x: Double, parameters: Double*): Array[Double] = {
              val a = parameters(0)
              val b = parameters(1)
              val powx2b = pow(x, 2 * b)
              val ga = - powx2b / pow(1 + a * powx2b, 2)
              val gb = - (2 * a * powx2b * log(x)) / pow(1 + a * powx2b, 2)
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
  /**
   *
   */
  private[clustering4ever] def generalSimplicialSetIntersection(simplicialSet1: DenseMatrix[Double], simplicialSet2: DenseMatrix[Double], weight: Double): DenseMatrix[Double] = {
      val result = simplicialSet1 + simplicialSet2
      Sparse.generalSSetIntersection(simplicialSet1, simplicialSet2, result, weight)
  }
  /** Under the assumption of categorical distance for the intersecting
    * simplicial set perform a fast intersection.
    *
    * @param simplicialSet: DenseMatrix[Double] The input fuzzy simplicial set
    * @param target: Array[Double] The categorical labels to use in the intersection.
    * @param unknownDist: Double The distance of an unknown label (-1) is assumed to be from any point.
    * @param farDist: Double The distance between unmatched labels.
    */
  def fastIntersection(simplicialSet: DenseMatrix[Double], target: Array[Double], unknownDist: Double = 1D, farDist: Double = 5D): DenseMatrix[Double] = {

      @annotation.tailrec
      def go(nz: Int, na: Int, ss: DenseMatrix[Double]): DenseMatrix[Double] = {
        if (nz < ss.rows) {
          if (na < ss.cols) {
              if (target(nz) == -1 || target(na) == -1) {
                ss(nz, na) *:*= exp(-unknownDist)
              }
              else if (target(na) != target(nz)) {
                ss(na, nz) *:*= exp(-farDist)
              }
              go(nz, na + 1, ss)
          }
          else go(nz + 1, 0, ss)
        }
        else ss
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
  private[clustering4ever] def resetLocalConnectivity(simplicialSet: DenseMatrix[Double]): DenseMatrix[Double] = {

      @annotation.tailrec
      def go(i: Int, normalizedSimpSet: DenseMatrix[Double]): DenseMatrix[Double] = {
          if (i < simplicialSet.rows) {
              normalizedSimpSet(i, ::) := simplicialSet(i, ::) / max(simplicialSet(i, ::))
              go(i + 1, normalizedSimpSet)
          }
          else normalizedSimpSet
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
    * @param simplicialSet: DenseMatrix[Double] The input fuzzy simplicial set
    * @param target: Array[Double] of shape nSamples The categorical labels to use in the intersection
    * @param unknownDist: Double (optional, default 1D) The distance of an unknown
    *                      label (-1) is assumed to be from any point
    * @param farDist    : Double (optional, Default 5D) The distance between unmatched labels
    * @return simplicialSet: DenseMatrix[Double] The resulting intersected fuzzy simplicial set
    */
  private[clustering4ever] def categoricalSimplicialSetIntersection(simplicialSet: DenseMatrix[Double], target: Array[Double], unknownDist: Double = 1D, farDist: Double = 5D): DenseMatrix[Double] = {

      val ss = fastIntersection(simplicialSet,target,unknownDist,farDist)
      resetLocalConnectivity(ss)
  }
  /**
   *
   */
  private[clustering4ever] def initTransform(indices: DenseMatrix[Int], weights: DenseMatrix[Double], embedding: DenseMatrix[Double]): DenseMatrix[Double] = {

      val result = DenseMatrix.zeros[Double](indices.rows, embedding.cols)
      for (i <- 0 until indices.rows) {
        for(j <- 0 until indices.cols) {
          for (d <- 0 until embedding.cols) {
            result(i, d) += weights(i, j) * embedding(indices(i, j), d)
          }
        }
      }
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
case class UMAP(val nNeighborsIn: Int = 15,
           val nComponents: Int = 2,
           val metric: Distance = new Euclidean,
           val nEpochs: Option[Int] = None,
           val learningRate: Double = 1.0,
           val init: UMAPInitialization = RandomInit,
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

    private var graph: Option[DenseMatrix[Double]] = None

    def fit(xData: DenseMatrix[Double], y: Option[DenseVector[Double]] = None): Option[UMAPModel] = {

        val (ares, bres) = UMAP.findABParams(spread, minDist)

        val (oneRow, nNeighbors) = if (xData.rows <= nNeighborsIn) {
            val oneRowIn = if (xData.rows == 1) true else false
            (oneRowIn, xData.rows - 1)
        }
        else (false, nNeighborsIn)

        if (!oneRow) {




            val (knni, knnd, rpf) = UMAP.nearestNeighbors(xData, nNeighbors, metric, angularRPForest)

            val g: DenseMatrix[Double] = UMAP.fuzzySimplicialSet(xData, nNeighbors, metric, Some(knni), Some(knnd), angularRPForest, setOPMixRatio, localConnectivity)

            /* Search Graph */
            val sgAux = new DenseMatrix[Int](xData.rows, xData.rows)
            for (i <- 0 until knni.rows) {
              for (j <- 0 until knni.cols) {
                sgAux(i, knni(i, j)) = knnd(i, j) match {
                    case 0D => 0
                    case _ => 1
                }
              }
            }

            val lessThanTranspose: DenseMatrix[Boolean] = sgAux <:< sgAux.t
            val sg = sgAux
            val valuesToMAJ = lessThanTranspose.findAll(a => a)
            valuesToMAJ.foreach(key => sg(key) = sgAux(key))

            val graph: Option[DenseMatrix[Double]] = if (y.isDefined) {
                // TODO: Exception a lever si jamais la taille des deux matrices n'est pas la meme
                val tm = targetMetric.getOrElse("categorical")
                tm match {
                    case "categorical" => {  
                      val fd = targetWeight match {
                          case x if x < 1D => 2.5 * (1D / (1D - targetWeight))
                          case _ => 1e12D
                      }
                      // graph = Some(UMAP.categoricalSimplicialSetIntersection(g, y.get.toArray, farDist = fd))
                      Some(UMAP.categoricalSimplicialSetIntersection(g, y.get.toArray, farDist = fd))
                    }
                    case _ => {  
                      val tnn = targetNNeighbors match {
                          case -1 => nNeighbors
                          case _ => targetNNeighbors
                      }
                      val targetGraph = UMAP.fuzzySimplicialSet(y.get.toDenseMatrix, tnn, targetMetric.getOrElse(new Euclidean), None, None, false, 1D, 1D, false)

                      // graph = Some(UMAP.generalSimplicialSetIntersection(g, targetGraph, targetWeight))
                      // graph = Some(UMAP.resetLocalConnectivity(g))
                      // Why
                      // UMAP.generalSimplicialSetIntersection(g, targetGraph, targetWeight)
                      Some(UMAP.resetLocalConnectivity(g))
                    }
                }
            }
            else None

            val nbEp = nEpochs.getOrElse(0)

            val embeddingOut = UMAP.simplicialSetEmbedding(g, nComponents, learningRate, ares, bres, repulsionStrength, negativeSampleRate, nbEp, init)

            Some(
              UMAPModel(
                learnedData = xData,
                embedding = embeddingOut,
                nNeighbors,
                knnIndices = knni,
                knnDists = knnd,
                rpForest = rpf,
                graphX = if (graph.isEmpty) g else graph.get,
                searchGraph = sg,
                a = ares,
                b = bres,
                nEpochs,
                repulsionStrength,
                negativeSampleRate,
                metric,
                localConnectivity,
                transformQueueSize,
                transformSeed,
                initialAlpha = learningRate
              )
            )

        }
        else None
    }

}

case class UMAPModel(
  val learnedData: DenseMatrix[Double],
  val embedding: DenseMatrix[Double],
  val nNeighbors: Int,
  val knnIndices: DenseMatrix[Int],
  val knnDists: DenseMatrix[Double],
  val rpForest: Forest,
  val graphX: DenseMatrix[Double],
  val searchGraph: DenseMatrix[Int],
  val a: Double,
  val b: Double,
  val nEpochs: Option[Int],
  val repulsionStrength: Double,
  val negativeSampleRate: Int,
  val metric: Distance,
  val localConnectivity: Double,
  val transformQueueSize: Double,
  val transformSeed: Int,
  val initialAlpha: Double
) {

    def transform(xData: DenseMatrix[Double]): DenseMatrix[Double] = {
        if (embedding.rows <= 1) throw new IllegalArgumentException("Transform unavailable with only a single data sample")

        val rand = new Random(transformSeed)
        val rangeState: Array[Long] = Array.fill(3) {rand.nextLong()}

        val init = NNDescent.initialiseSearch(rpForest, learnedData, xData, (transformQueueSize  * nNeighbors).toInt, rangeState, metric)
        val search = NNDescent.makeInitialisedNNDSearch(metric) _
        val res = search(learnedData, init, xData)

        val (allindices, alldists) = res.deheapSort
        val indices = allindices(::, 0 until nNeighbors)
        val dists = alldists(::, 0 until nNeighbors)
        val adjustedLocalConnectivity = max(0D, localConnectivity - 1D)
        val smooth = UMAP.smoothKNNDist(dists, nNeighbors, localConnectivity = adjustedLocalConnectivity)

        val (rows, cols, vals) = UMAP.membershipStrengths(indices, dists, smooth._1, smooth._2)
        val graph = Utils.makeMatrix(rows, cols, vals, xData.rows, learnedData.rows)


        val normalizedGraph = normalize(graph, Axis._1, 1)
        val wei = DenseVector((for (i <- 0 until rows.length) yield normalizedGraph(rows(i), cols(i))): _*)
        val inds = cols.toDenseMatrix.reshape(nNeighbors, xData.rows).t
        val weights = wei.toDenseMatrix.reshape(nNeighbors, xData.rows).t

        val emb = UMAP.initTransform(indices, weights, embedding)
        val nbEp = nEpochs.map(_ / 3).getOrElse(if (graph.rows <= 10000) 100 else 30)
        val limit = max(graph) / nbEp.toDouble
        val lessThanLimit = graph.findAll(n => n < limit && n != 0)
        lessThanLimit.foreach(key => graph(key) = 0D)
        val nzv = Sparse.nonZeroValues(graph)
        val nzr = Sparse.nonZeroRows(graph)
        val nzc = Sparse.nonZeroCols(graph)
        val epochsPerSample = UMAP.makeEpochsPerSample(nzv, nbEp)

        val head = DenseVector(nzr)
        val tail = DenseVector(nzc)

        UMAP.optimizeLayout(emb, embedding, head, tail, nbEp, graph.cols, epochsPerSample, a, b, Array(2, 1, 1), repulsionStrength, initialAlpha, negativeSampleRate
        )
    }

}