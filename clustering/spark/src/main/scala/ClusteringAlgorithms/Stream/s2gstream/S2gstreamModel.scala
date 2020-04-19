package org.clustering4ever.spark.clustering.s2gstream
/**
 * @author Attaoui Walid
 * @author Beck Gaël
 * @author Ghesmoune Mohammed
 */
import java.io.Serializable
import breeze.linalg.{DenseVector, Vector, squaredDistance}
import breeze.numerics.pow
import org.apache.spark.rdd.RDD
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.exp
import org.clustering4ever.spark.streamclustering.{Prototype, PointObj}

class S2gstreamModel(
  var nodes: mutable.ArrayBuffer[Prototype],
  val outdatedNodes: mutable.ArrayBuffer[Prototype],
  val isolatedNodes: mutable.ArrayBuffer[Prototype],
  var edges: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]],
  var ages: mutable.ArrayBuffer[mutable.ArrayBuffer[Double]],
  var errors: mutable.ArrayBuffer[Double],
  val alpha: mutable.ArrayBuffer[mutable.ArrayBuffer[Double]] ,
  val beta: mutable.ArrayBuffer[mutable.ArrayBuffer[Double]] ,
  var clusterWeights: mutable.ArrayBuffer[Double]
) extends Serializable {


  def this() = this(
    nodes = mutable.ArrayBuffer.empty[Prototype],
    outdatedNodes = mutable.ArrayBuffer.empty[Prototype],
    isolatedNodes = mutable.ArrayBuffer.empty[Prototype],
    edges = mutable.ArrayBuffer.empty[mutable.ArrayBuffer[Int]],
    ages = mutable.ArrayBuffer.empty[mutable.ArrayBuffer[Double]],
    errors = mutable.ArrayBuffer.empty[Double],
    alpha = mutable.ArrayBuffer.empty[mutable.ArrayBuffer[Double]],
    beta = mutable.ArrayBuffer.empty[mutable.ArrayBuffer[Double]],
    clusterWeights = mutable.ArrayBuffer.empty[Double]
  )

  class variable() {
    var bloc: Int = 0
  }

  // initialize the model with 2 NodesObj
  def init2NodesObj(
    n1: Array[Double],
    n2: Array[Double],
    dim: Int,
    idPoint: Int
  ): S2gstreamModel = {

    nodes += (pointToProto(n1, dim, idPoint))
    nodes += (pointToProto(n2, dim, idPoint + 1))
    edges = mutable.ArrayBuffer(mutable.ArrayBuffer(0, 1), mutable.ArrayBuffer(1, 0))
    ages = mutable.ArrayBuffer(mutable.ArrayBuffer(Double.NaN, 0), mutable.ArrayBuffer(0, Double.NaN))
    errors = mutable.ArrayBuffer(0, 0)
    clusterWeights = mutable.ArrayBuffer(1, 1)
    this

  }

  // point To Object
  def pointToObjet(e: Array[Double], dim: Int, labId: Int) = {
    val dataPart = e.take(e.size - labId) //labId=-2 because the 2 last columns represent labels & id
    val part1 = Vector(dataPart.take(dim))
    val etiq = e.drop(dim).map(_.toInt)
    PointObj(part1, etiq(0), etiq(1))
  }

  // pointToProto
  def pointToProto(e: Array[Double], dim: Int, idPoint: Int) = {
    val dataPart = e.take(e.size - 2) //-2 because the 2 last columns represent labels & id
    val part1 = Vector(dataPart.take(dim))
    Prototype(part1, Set(idPoint), nodes.length + 1)
  }

  def getLabels(rdd: RDD[PointObj]): Array[Int] = {
    val labels = rdd.map(point => findTwoNearestPointDist1L(nodes, point)._1).collect()
    labels
  }

  def updateFeatureWeight(
    rdd: RDD[PointObj],
    gstream: S2gstream,
    alpha2: Array[Array[Double]],
    beta2: Array[Array[Double]],
    alphagen:Array[Double],
    betagen: Array[Double],
    dim: Int,
    b: Int,
    lambda: Double,
    eta: Double,
    bloc: Array[Int],
    p: Array[Int],
    rddcount: Int
  ): S2gstreamModel = {
    //feature weights


    rdd.cache()
    val withIndex = rdd.zipWithIndex
    val indexKey = withIndex.map(_.swap)
    for (l <- 0 until rddcount) {
      val point = indexKey.lookup(l).head
      val clos = findTwoNearestPointDist3(nodes, point,alphagen,betagen, b, bloc)._1
      val centroid = nodes(clos).protoPartNum
      for (j <- 0 until dim) {
        var valpha = alpha2(clos)(bloc(j))
        var dist = pow(centroid(j) - point.pointPartNum(j), 2)

        //local
        beta2(clos)(j) += valpha * dist
        beta2(clos)(j) = exp(-beta2(clos)(j) / eta)

        //global
        valpha = alphagen(bloc(j))
        betagen(j) += valpha * dist
        betagen(j) = exp(-betagen(j) / eta)
      }
    }

    for (i <- 0 until nodes.size){

      val sums = Array.fill(b)(0D)

      for (j <- 0 until dim) {
        sums(bloc(j)) += beta2(i)(j)
      }

      for (j <- 0 until dim) {
        beta2(i)(j) /= sums(bloc(j))
      }

    }

    val sums = Array.fill(b)(0D)

    for (j <- 0 until dim) {
      sums(bloc(j)) += betagen(j)
    }

    for (j <- 0 until dim) {
      betagen(j) /= sums(bloc(j))
    }

    this

  }

  def updateGroupWeight(
  rdd: RDD[PointObj],
  gstream: S2gstream,
  alpha2: Array[Array[Double]],
  beta2: Array[Array[Double]],
  alphagen:Array[Double],
  betagen:Array[Double],
  dim: Int,
  b: Int,
  lambda: Double,
  eta: Double,
  bloc: Array[Int],
  p: Array[Int],
  rddcount: Int
): S2gstreamModel = {



    //update subspace weights

    rdd.cache()
    for (l <- 0 until rddcount) {
      val withIndex = rdd.zipWithIndex
      val indexKey = withIndex.map(_.swap)
      val point = indexKey.lookup(l).head
      val clos = findTwoNearestPointDist1L(nodes, point)._1 //findTwoNearestPointDist2(nodes, point, alpha2, beta2, b, lambda, eta, bloc, p)._1
      val clos2 =  findTwoNearestPointDist1L(nodes, point)._1 //findTwoNearestPointDist2(nodes, point, alpha2, beta2, b, lambda, eta, bloc, p)._1
      val centroid = nodes(clos).protoPartNum
      val centroid2 = nodes(clos2).protoPartNum

      for (t <- 0 until b) {
        var sum2 = 0D
        for (k <- 0 until dim) {
          if (bloc(k) == t) {

            var vbeta = beta2(clos)(k)
            var dist = pow(centroid(k) - point.pointPartNum(k), 2)
            alpha2(clos)(t) += vbeta * dist
            alphagen(t) += vbeta * dist
          }
        }


      }
    }

    for (i <- 0 until nodes.size){
      for (j <- 0 until b) {
        alpha2(i)(j) = exp(-alpha2(i)(j) / lambda)
      }
    }

    for (j <- 0 until b) {
      alphagen(j) = exp(-alphagen(j) / lambda)
    }

    for (i <- 0 until nodes.size) {

      var sum = alpha2(i).sum

      for (j <- 0 until b) {
        if(sum == 0) {
          alpha2(i)(j) /= sum
        }
        else{
          alpha2(i)(j) = 0
        }
      }

    }
    var sum = alphagen.sum
    for (j <- 0 until b) {
      if (sum == 0) {
        alphagen(j) /= sum
      }
      else{
        alphagen(j) = 0
      }
    }

    this
  }

  // updateObj : the main update method
  def updateObj(
    rdd: RDD[PointObj],
    gstream: S2gstream,
    kk: Int,
    dim: Int,
    alpha2: Array[Array[Double]],
    beta2: Array[Array[Double]],
    alphagen:Array[Double],
    betagen:Array[Double],
    b: Int,
    lambda: Double,
    eta: Double,
    bloc: Array[Int],
    p: Array[Int]
  ): S2gstreamModel = {

    // the mapping step
    val closestNum = rdd.map( point => findTwoNearestPointDist1L(nodes, point) )

    // get sums and counts for updating each cluster
    val mergeContribs: ((Array[Int], Double, Vector[Double], Long, Set[Int]),
      (Array[Int], Double, Vector[Double], Long, Set[Int])) => (Array[Int], Double, Vector[Double], Long, Set[Int]) = {
      case ((bmu1, error1, sum1, count1, idsData1), (bmu2, error2, sum2, count2, idsData2)) =>
        val bmu = addPairwiseInt(bmu1, bmu2)
        (bmu, error1 + error2, sum1 + sum2, count1 + count2, idsData1 union idsData2)
    }


    val dim = nodes(0).protoPartNum.size
    val nbNodes = nodes.size

    val neutralElem = (Array.fill(nbNodes)(0), 0D, Vector.zeros[Double](dim), 0L, Set.empty[Int])
    val pointStats: Array[(Int, (Array[Int], Double, Vector[Double], Long, Set[Int]))] = closestNum
      .aggregateByKey(neutralElem)(mergeContribs, mergeContribs)
      .collect()

    // implement update rule
    updateRule(pointStats, kk, dim, gstream.decayFactor, gstream.lambdaAge, gstream.voisinage)

    // remove edges having an age > maxAge
    removeOldEdges(gstream.maxAge)

    // delete each isolated node
    removeIsolatedNodes()

    // update the global error
    upGlobalErrors(pointStats)

    // applying the fading function
    if (nbNodes > 100 & kk % 3 == 0) {
      fading(gstream.minWeight)
    }

    // delete each isolated node
    removeIsolatedNodes()

    // add new nodes --x2 or x3
    if (nbNodes <= 300 & kk % 5 == 0) {
      addNewNodes(gstream.nbNodesToAdd, kk, dim, gstream.alphaErr)
    }

    // decrease the error of all units.
    for (i <- 0 until errors.size) errors(i) *= gstream.d

    this

  }


  // find the two nearest nodes ti the input data-point
  def findTwoNearestPointDist1L(nodes: mutable.ArrayBuffer[Prototype], input: PointObj) = {
    val nbNodes = nodes.length
    var distances = mutable.ArrayBuffer.empty[Double]
    for (i <- 0 until nbNodes) {
      distances += (squaredDistance(input.pointPartNum, nodes(i).protoPartNum))
    }
    val (sdistances, indices) = distances.zipWithIndex.sorted.unzip
    val clos1 = indices(0)
    val clos2 = indices(1)

    val clos2VecBinaire = Array.fill(nbNodes)(0)
    clos2VecBinaire(clos2) = 1

    (clos1, (clos2VecBinaire, sdistances(0), input.pointPartNum, 1L, Set(input.id)))
  }

  def findTwoNearestPointDist2(
    nodes: mutable.ArrayBuffer[Prototype],
   input: PointObj,
   alpha2: Array[Array[Double]],
   beta2: Array[Array[Double]],
   block: Int,
   lambda: Double,
   eta: Double,
   bloc: Array[Int],
   p: Array[Int]
) = {

    var nbNodes = nodes.length
    var distances = mutable.ArrayBuffer.empty[Double]

    for (i <- 0 until nbNodes) {
      distances += (squaredDistance(input.pointPartNum, nodes(i).protoPartNum))
    }
    for (c <- 0 until nbNodes) {
      for (a <- 0 until block) {
        var sum = 0D
        for (j <- 0 until p(a)) {

          sum += (beta2(c)(j) * distances(c))

        }
        distances(c) = (alpha2(c)(a) * sum)
      }


    }
    val (sdistances, indices) = distances.zipWithIndex.sorted.unzip
    val clos1 = indices(0)
    val clos2 = indices(1)

    val clos2VecBinaire = Array.fill(nbNodes)(0)
    clos2VecBinaire(clos2) = 1

    (clos1, (clos2VecBinaire, sdistances(0), input.pointPartNum, 1L, Set(input.id)))
  }

  def findTwoNearestPointDist3(
         nodes: mutable.ArrayBuffer[Prototype],
         input: PointObj,
         alphagen:Array[Double],
         betagen:Array[Double],
         block: Int,
         bloc: Array[Int]
       ) = {

    var nbNodes = nodes.length
    var distances = mutable.ArrayBuffer.empty[Double]
    for (i <- 0 until nbNodes) {
      distances += (euclideanDistance(input.pointPartNum.toArray, nodes(i).protoPartNum.toArray,alphagen,betagen, block, bloc))
    }
    val (sdistances, indices) = distances.zipWithIndex.sorted.unzip
    val clos1 = indices(0)
    val clos2 = indices(1)

    var clos2VecBinaire = Array.fill(nbNodes)(0)
    clos2VecBinaire(clos2) = 1

    (clos1, (clos2VecBinaire, sdistances(0), input.pointPartNum, 1L, Set(input.id)))



  }

  // euclideanDistance
  def euclideanDistance(
  a: Array[Double],
  b: Array[Double],
  alphagen:Array[Double],
  betagen:Array[Double],
  block: Int,
  bloc: Array[Int]
): Double = {

    var sum = 0D
    var i = 0
    while (i < a.size) {
      val toPow2 = a(i) - b(i)
      sum += alphagen(bloc(i)) * betagen(i) * toPow2 * toPow2
      i += 1
    }
    math.sqrt(sum)
  }

  // implement update rule
  def updateRule(
    pointStats: Array[(Int, (Array[Int], Double, Vector[Double], Long, Set[Int]))],
    kk: Int,
    dim: Int,
    decayFactor: Double,
    lambdaAge: Double,
    voisinage: Int
   ) = {


    val discount  = decayFactor
    // apply discount to weights
    clusterWeights = scal(discount, clusterWeights)

    pointStats.foreach{ case (label, (bmu2, errs, sum, count, idsData)) =>
      //pointStats: Array[(Int, (Array[Int], Double, Vector[Double], Long, Set[Int])
      // increment the age of all edges emanating from s1.
      val s1Neighbors =  edges(label).zipWithIndex.collect{ case (a, idx) if a == 1 => idx }
      val sizeOfNeighborhood = s1Neighbors.size
      // ages(s1Neighbors,s1) = ages(s1Neighbors,s1)+age_inc
      for(i <- 0 until sizeOfNeighborhood) {
        ages(s1Neighbors(i))(label) *= lambdaAge
        ages(s1Neighbors(i))(label) += 1
        ages(label)(s1Neighbors(i)) = ages(s1Neighbors(i))(label)

      }

      // merge old ids with the ids of the last window
      nodes(label).idsDataAssigned = nodes(label).idsDataAssigned union idsData

      val v1 = scal(clusterWeights(label), nodes(label).protoPartNum)
      var nominateur = v1 + sum
      var denominateur = clusterWeights(label) + count

      if (voisinage == 1) {
        var tsumi: Vector[Double] = Vector.zeros[Double](dim)
        var tcounti = 0D
        val labelNeighbors = edges(label).zipWithIndex.collect{ case (a, idx) if a == 1 => idx }

        labelNeighbors.foreach{ e =>
          val closestProto = pointStats.filter{ case (point, _) => point == e }
          if (closestProto.length > 0) {

            val f1: (Long, (Int, (Array[Int], Double, Vector[Double], Long, Set[Int]))) => Long = {
 case (count, (_, (_, _, _, countPoint, _))) => count + countPoint
            }
            val counti = closestProto.foldLeft(0L)(f1)

            val f2: (Vector[Double], (Int, (Array[Int], Double, Vector[Double], Long, Set[Int]))) => Vector[Double] = {
 case (sum, (_, (_, _, sumPoint, _, _))) => sum + sumPoint
            }
            val sumi = closestProto.foldLeft(Vector.fill(dim)(0D))(f2)
            val k = kNeighbor(nodes(label), nodes(e), temperature)

            tsumi += k * sumi
            tcounti += k * counti

          }
        }

        nominateur += tsumi
        denominateur += tcounti

      }

      val ctplus1 = scal(1D / Math.max(denominateur, 1e-16), nominateur)

      // move each centroid
      clusterWeights(label) = clusterWeights(label) + count
      nodes(label).protoPartNum = ctplus1


      val idxMaxValue = bmu2.zipWithIndex.maxBy(_._1)._2

      // create an edge (label,idxMaxValue)
      // if s1 and s2 are connected by an edge, set the age of this edge to zero. If such an edge does not exist, create it.
      edges(label)(idxMaxValue) = 1
      edges(idxMaxValue)(label) = 1
      ages(label)(idxMaxValue) = 0
      ages(idxMaxValue)(label) = 0

      // update the error variable
      errors(label) += errs

    } //end update-rule
  }

  // remove old edges
  def removeOldEdges(maxAge: Int) = {
    val delRowCol: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]] = mutable.ArrayBuffer()
    for (i <- 0 until ages.size) {
      delRowCol += ages(i).zipWithIndex.collect{ case (age, idx) if age > maxAge => idx }
    }
    val sizedelRowCol = delRowCol.size
    var delCol = mutable.ArrayBuffer.empty[Int]
    for (i <- 0 until sizedelRowCol) {
      delCol = delRowCol(i)
      delCol.foreach{ x =>
        edges(i)(x) = 0
        ages(i)(x) = Double.NaN
      }
    }
  }

  // delete each isolated node
  def removeIsolatedNodes() = {

    val nbrNodes = nodes.size
    var nbNodesAct = edges.head.size
    if (nbrNodes != nbNodesAct) {
      throw new IllegalStateException("The size of nodes and edges must be equal, edges must be a square matrix")
    }

    for (j <- nbrNodes - 1 until 0 by -1) {
      if (edges(j).equals(mutable.ArrayBuffer.fill(nbNodesAct)(0))) {

        // save this isolated node
        isolatedNodes += nodes(j)

        // delete
        edges = removeLineColInt(j, edges)
        ages = removeLineCol(j, ages)
        nodes.remove(j)
        clusterWeights.remove(j)
        errors.remove(j)

        nbNodesAct -= 1
      }
    }
  }
  /**
    * Update the global errors
    */
  def upGlobalErrors(pointStats: Array[(Int, (Array[Int], Double, Vector[Double], Long, Set[Int]))]) = {
    val erreurs = pointStats.groupBy(_._1).mapValues(_.foldLeft(0D)(_ + _._2._2)).toArray //erreurs: (label, error)
    for ((er1, er2) <- erreurs) {
      if (errors.size < er1)
        errors(er1) += er2
    }
  }

  // add new nodes --x3
  def addNewNodes(nbNodesToAdd: Int, kk: Int, dim: Int, alphaErr: Double) = {
    for (j <- 1 until nbNodesToAdd) {
      // find the node with the largest error
      val q = errors.indexOf(errors.max)

      //val (errs, idxErr) = erreurs.zipWithIndex.sorted.unzip
      val qNeighbors = edges(q).zipWithIndex.collect{ case (a, idx) if a == 1 => idx }



      // find the neighbor f with the largest accumulated error
      val indexToLookFor = errors.zipWithIndex.collect{ case (a, idx) if qNeighbors.exists(_ == idx) => a }.max
      val f = errors.indexOf(indexToLookFor)
      // Add the new node half-way between nodes q and f: nodes = [nodes .5*(nodes(:,q)+nodes(:,f))]
      if (q != -1 && f != -1) {
        val node1 = nodes(q).protoPartNum + nodes(f).protoPartNum
        node1 :*= 0.5
        val protoNode1 = Prototype(node1, /*Vector.zeros(dim - sizeNumPart),*/ Set(), nodes.length + 1)
        nodes += (protoNode1)

        clusterWeights += (0)

        // Remove the original edge between q and f.
        edges(q)(f) = 0
        edges(f)(q) = 0
        ages(q)(f) = Double.NaN
        ages(f)(q) = Double.NaN
        // insert edges connecting the new unit r with units q and f.
        val r = edges.size
        edges = addElementLast(0, edges)
        edges(q)(r) = 1
        edges(r)(q) = 1
        edges(f)(r) = 1
        edges(r)(f) = 1

        ages = addElementLast(Double.NaN, ages)
        ages(q)(r) = 0
        ages(r)(q) = 0
        ages(f)(r) = 0
        ages(r)(f) = 0

        errors(q) = errors(q) * alphaErr
        errors(f) = errors(f) * alphaErr
        errors += ((errors(q) + errors(f)))
      }
    }
  }

  /**
    * fading function
    */
  def fading(minWeight: Double) = {

    if (nodes.size != clusterWeights.size) {
      throw new IllegalStateException("The size of nodes and weights must be equal !")
    }

    val weightsWithIndex = clusterWeights.view.zipWithIndex
    val (minW, smallest) = weightsWithIndex.minBy(_._1)
    if (minW < minWeight) {
      // save this node as an outdated node
      outdatedNodes += nodes(smallest)

      // delete
      edges = removeLineColInt(smallest, edges)
      ages = removeLineCol(smallest, ages)
      clusterWeights.remove(smallest)
      errors.remove(smallest)
      nodes.remove(smallest)
    }
  }

  // temperature
  def temperature() = {
    0.3
  }

  // the K-Neighborhood function
  def kNeighbor(neuron1: Prototype, neuron2: Prototype, temperature: Double): Double = {
    exp(-1D / temperature) //the vicinity !
  }

  // the K-Neighborhood function
  def kNeighborSOM(neuron1: Prototype, neuron2: Prototype, temperature: Double): Double = {
    exp(-(squaredDistance(neuron1.protoPartNum, neuron2.protoPartNum)) / temperature)
  }


  // add a new line and a new column containing each an array of element e
  def addElementLast(e: Double, a: mutable.ArrayBuffer[mutable.ArrayBuffer[Double]]) = {
    val b = a
    if (a.size == 0) b += mutable.ArrayBuffer.fill(1)(e)
    else {
      b += mutable.ArrayBuffer.fill(a(0).size)(e)
      b.foreach { _ += (e) }
      b
    }
  }

  def addElementLast(e: Int, a: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]]) = {
    val buffer: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]] = a
    if (a.size == 0) buffer += mutable.ArrayBuffer.fill(1)(e)
    else {
      buffer += mutable.ArrayBuffer.fill(a(0).size)(e)
      buffer.foreach(_ += e)
      buffer
    }
  }


  // remove the ith line and the ith column
  def removeLineCol(i: Int, a: mutable.ArrayBuffer[mutable.ArrayBuffer[Double]]): mutable.ArrayBuffer[mutable.ArrayBuffer[Double]] = {
    val b = a
    b.remove(i)
    b.foreach(_.remove(i))
    b
  }

  def removeLineColInt(i: Int, a: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]]): mutable.ArrayBuffer[mutable.ArrayBuffer[Int]] = {
    val b = a
    b.remove(i)
    b.foreach(_.remove(i))
    b
  }

  //  res = a x b
  def scal(a: Double, b: Array[Double]): Array[Double] = {
    b.zipWithIndex.collect{ case (x, y) => a * x }
  }
  def scal(a: Double, b: mutable.ArrayBuffer[Double]): mutable.ArrayBuffer[Double] = {
    b.zipWithIndex.collect{ case (x, y) => a * x }
  }
  def scal(a: Double, b: Vector[Double]): Vector[Double] = {
    Vector(scal(a, b.toArray))
  }

  def addPairwise(a: Array[Double], b: Array[Double]): Array[Double] = {
    (a zip b).map{ case (x, y) => x + y }
  }

  def addPairwiseInt(a: Array[Int], b: Array[Int]): Array[Int] = {
    val output = Array.ofDim[Int](a.size)
    var i = 0
    while (i < a.size) {
      output(i) = a(i) + b(i)
      i += 1
    }
    output
  }
  def addPairwise(a: mutable.ArrayBuffer[Double], b: mutable.ArrayBuffer[Double]): mutable.ArrayBuffer[Double] = {
    (a zip b).map{ case (x, y) => x + y }
  }

  //  res = a*x+y
  def axpy(a: Double, x: Array[Double], y: Array[Double]) = {
    addPairwise(scal(a, x), y)
  }
  def axpy(a: Double, x: Vector[Double], y: Vector[Double]) = {
    x :*= a
    x + y
  }
  def axpy(a: Int, x: Vector[Int], y: Vector[Int]) = {
    x :*= a
    x + y
  }


  /**
    * Test if two vectors are equals
    */
  def areQuasiEqual(a: Vector[Double], b: Vector[Double], epsilon: Double = 1e-10): Boolean = {

    @annotation.tailrec
    def areQuasiEqualIn(i: Int, ok: Boolean): Boolean = {
      if (ok && i < a.length) {
        val ip1 = i + 1
        if (Math.abs(a(i) - b(i)) > epsilon) areQuasiEqualIn(ip1, false)
        else areQuasiEqualIn(ip1, ok)
      }
      else ok
    }

    if (a.length != b.length) false
    else areQuasiEqualIn(0, true)

  }
  def findClosestProto(nodes: mutable.ArrayBuffer[Prototype], input: Array[Double]) = {
    var nbProto = nodes.length
    //var protowithindex = proto.collect()
    var distance = Double.MaxValue
    var closest_index = 0
    var closest_proto = nodes(0)
    for (i <- 0 until nbProto){
      var dist = squaredDistance(DenseVector(input), nodes(i).protoPartNum)
      if( dist < distance){
        distance = dist
        closest_index = i
        closest_proto = nodes(i)
      }
    }
    closest_index
  }


  // toString methods
  override def toString = nodes.toArray.deep.mkString("\n")
  def toStringIds = nodes.map(_.toStringIds)
  def toStringOutdatedIds = outdatedNodes.map(_.toStringIds)
  def toStringProto = nodes.map(_.toStringProto)
  def toStringCard = nodes.map(_.toStringCard)
  def toStringAss = nodes.map(_.toStringAss)
  def toStringOutdatedAss = outdatedNodes.map(_.toStringAss)
  def toStringOutdatedProto = outdatedNodes.map(_.toStringProto)
  def toStringIdNode = nodes.map(_.toStringId)
  def toStringIdOutdated = outdatedNodes.map(_.toStringId)

}
