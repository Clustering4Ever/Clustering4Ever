package org.clustering4ever.spark.clustering.batchstream
	 /**
	  * Copyright: please refer to the README.md file
	  * User: ghesmoune
	  * Date: 01/01/2016
	  * Project : Square Predict (http://square-predict.net/)
	  * */ 
import scala.collection.mutable
import breeze.linalg.{Vector, squaredDistance}
import org.apache.spark.rdd.RDD
import scala.math.{abs, exp}
import scala.reflect.ClassTag
import org.clustering4ever.spark.streamclustering.{Prototype, PointObj}

class BatchStreamModel(
    var nodes: mutable.ArrayBuffer[Prototype], 
    var outdatedNodes: mutable.ArrayBuffer[Prototype], 
    var isolatedNodes: mutable.ArrayBuffer[Prototype], 
    var edges: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]], 
    var ages: mutable.ArrayBuffer[mutable.ArrayBuffer[Double]], 
    var errors: mutable.ArrayBuffer[Double], 
    var clusterWeights: mutable.ArrayBuffer[Double] 
    ) extends Serializable {
   
  
  def this() = this(
      nodes = mutable.ArrayBuffer(),
      outdatedNodes = mutable.ArrayBuffer(),
      isolatedNodes = mutable.ArrayBuffer(),
      edges = mutable.ArrayBuffer(),
      ages = mutable.ArrayBuffer(),
      errors = mutable.ArrayBuffer(),
      clusterWeights = mutable.ArrayBuffer()
      )
  
  // initialize the model with 2 NodesObj    
  def init2NodesObj(n1: Array[Double], n2: Array[Double], dim: Int, idPoint: Int): BatchStreamModel = {
    this.nodes += this.pointToProto(n1, dim, idPoint)
    this.nodes += this.pointToProto(n2, dim, idPoint+1)
    this.edges = mutable.ArrayBuffer(mutable.ArrayBuffer(0,1),mutable.ArrayBuffer(1,0))
    this.ages = mutable.ArrayBuffer(mutable.ArrayBuffer(Double.NaN,0),mutable.ArrayBuffer(0,Double.NaN))
    this.errors = mutable.ArrayBuffer(0,0)
    this.clusterWeights = mutable.ArrayBuffer(1, 1)
    this
  }
  
  // point To Object
	def pointToObjet(e: Array[Double], dim: Int, labId: Int) = {
    val dataPart = e.take(e.size - labId) //labId=-2 because the 2 last columns represent labels & id
    val part1 = Vector(dataPart.take(dim))
    val etiq = e.drop(dim).map(_.toInt)
    new PointObj(part1, etiq(0), etiq(1))
  } 
   
	// pointToProto
	def pointToProto(e: Array[Double], dim: Int, idPoint: Int) = {
    val dataPart = e.take(e.size - 2) //-2 because the 2 last columns represent labels & id
    val part1 = Vector(dataPart.take(dim))
    new Prototype(part1, Set(idPoint), this.nodes.length+1) 
  } 	
	
	// updateObj : the main update method
  def updateObj(rdd: RDD[PointObj], gstream: BatchStream, kk: Int, dim: Int): BatchStreamModel = {
    // the mapping step     
    val closestNum = rdd.map(point => this.findTwoNearestPointDist1L(this.nodes, point)) 
       
    // get sums and counts for updating each cluster
		val mergeContribs: ((Array[Int], Double, Vector[Double], Long, Set[Int]), (Array[Int], Double, Vector[Double], Long, Set[Int])) =>
		  (Array[Int], Double, Vector[Double], Long, Set[Int]) = (p1, p2) => {
		    val yP11 = addPairwise(p2._1, p1._1)
		    (yP11, p1._2 + p2._2, p2._3 + p1._3, p1._4 + p2._4, p1._5 union p2._5)
		  }
	
		val dim = this.nodes(0).protoPartNum.size
		val nbNodes = this.nodes.size
		
		// the reduce step 
	  val pointStats: Array[(Int,(Array[Int], Double, Vector[Double], Long, Set[Int]))] = closestNum.aggregateByKey((Array.fill(nbNodes)(0), 0.0, Vector.zeros[Double](dim), 0L, Set[Int]()))(mergeContribs, mergeContribs)
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
		if( nbNodes > 100 & kk % 3 == 0 ) fading(gstream.minWeight)
		
		// delete each isolated node
    removeIsolatedNodes()    
		
		// add new nodes --x2 or x3
    if( nbNodes <= 300 & kk % 5 == 0 ) addNewNodes(gstream.nbNodesToAdd, kk, dim, gstream.alphaErr)
		
		// decrease the error of all units.
    for( i <- 0 until this.errors.size ) this.errors(i) *= gstream.d
      
    this  
  }

  
  // find the two nearest nodes ti the input data-point
  def findTwoNearestPointDist1L(nodes: mutable.ArrayBuffer[Prototype], input: PointObj) = {
    var nbNodes = nodes.length 
    var distances: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer()
    for (i<-0 to nbNodes-1){
      distances.+=(squaredDistance(input.pointPartNum, nodes(i).protoPartNum)) 
    }
    val (sdistances, indices) = distances.zipWithIndex.sorted.unzip
    val clos1 = indices(0)
    val clos2 = indices(1)
    
    var clos2VecBinaire = Array.fill(nbNodes)(0)  
    clos2VecBinaire(clos2) = 1
        
    (clos1, (clos2VecBinaire, sdistances(0), input.pointPartNum, 1L, Set(input.id))) 
  }

  // implement update rule 
  def updateRule(pointStats: Array[(Int, (Array[Int], Double, Vector[Double], Long, Set[Int]))], 
      kk: Int, dim: Int, decayFactor: Double, lambdaAge: Double, voisinage: Int) = {
    val discount  = decayFactor	  
	  // apply discount to weights
		this.clusterWeights = this.scal(discount, this.clusterWeights)
		
		pointStats.foreach { case (label, (bmu2, errs, sum, count, idsData)) =>
		  
		  // increment the age of all edges emanating from s1.
		  val s1_Neighbors = this.edges(label).zipWithIndex.collect{ case (v, idx) if v == 1 => idx }
		  val SizeOfNeighborhood = s1_Neighbors.size
		  for( i <- 0 until SizeOfNeighborhood ) {
		    this.ages(s1_Neighbors(i))(label) *= lambdaAge
		    this.ages(s1_Neighbors(i))(label) += 1
		    this.ages(label)(s1_Neighbors(i)) = this.ages(s1_Neighbors(i))(label)
		  }

		  this.nodes(label).idsDataAssigned = this.nodes(label).idsDataAssigned union idsData
		  
		  val v1 = scal(this.clusterWeights(label), this.nodes(label).protoPartNum)
		  var nominateur = v1 + sum
		  var denominateur = this.clusterWeights(label) + count

		  if (voisinage == 1) {
			  var tsumi :Vector[Double] = Vector.zeros[Double](dim) 		    
			  var tcounti = 0D
			  val labelNeighbors = this.edges(label).zipWithIndex.collect{ case (v, idx) if v == 1 => idx }
			  labelNeighbors.foreach{ e =>
				  val tmp = pointStats.filter(_._1 == e)
				  if (tmp.length > 0) {
					  val counti = tmp.map(_._2._4).sum
					  val sumi = tmp.map(_._2._3).reduceLeft[Vector[Double]](_ + _)
					  val k = this.kNeighbor(this.nodes(label), this.nodes(e), this.temperature())
					  tsumi += k * sumi 					  
					  tcounti += k * counti
				  }
			  }
			  nominateur += tsumi
			  denominateur += tcounti
		  }

		  val ctplus1 = scal(1 / math.max(denominateur, 1e-16), nominateur)
		    
      // move each centroid		  
      this.clusterWeights(label) = this.clusterWeights(label) + count
 		  this.nodes(label).protoPartNum = ctplus1
 
		  
		  val idxMaxValue = bmu2.zipWithIndex.maxBy(_._1)._2
			  
		  // create an edge (label,idxMaxValue)
      // if s1 and s2 are connected by an edge, set the age of this edge to zero. If such an edge does not exist, create it.
      this.edges(label)(idxMaxValue) = 1
      this.edges(idxMaxValue)(label) = 1
      this.ages(label)(idxMaxValue) = 0
      this.ages(idxMaxValue)(label) = 0
      
      // update the error variable
      this.errors(label) += errs
		  
		} //end update-rule
  }

  // remove old edges   
  def removeOldEdges(maxAge: Int) = {
    val delRowCol: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]] = mutable.ArrayBuffer()
    	(0 until this.ages.size).foreach( i => delRowCol += this.ages(i).zipWithIndex.collect{ case (v, idx) if v > maxAge => idx } )
    	val sizeDelRowCol = delRowCol.size
    	(0 until sizeDelRowCol).foreach{ i =>
      	delRowCol(i).foreach{ x =>
          this.edges(i)(x) = 0  
          this.ages(i)(x) = Double.NaN
        }
    	}
  }

	// delete each isolated node
  def removeIsolatedNodes() = {
    val nbrNodes = this.nodes.size
    var nbNodesAct = this.edges.head.size 
    if (nbrNodes != nbNodesAct) throw new IllegalStateException("The size of nodes and edges must be equal, edges must be a square matrix")

		for ( j <- nbrNodes - 1 to 0 by -1 ) {
			if (this.edges(j).equals(mutable.ArrayBuffer.fill(nbNodesAct)(0))) { 
			  // save this isolated node
			  this.isolatedNodes += this.nodes(j)
			  // delete
				this.edges = this.removeLineColInt(j, this.edges)
				this.ages = this.removeLineCol(j, this.ages)
				this.nodes.remove(j)
				this.clusterWeights.remove(j)
				this.errors.remove(j)
				nbNodesAct -= 1
			}
		}
  }

  // update the global errors
  def upGlobalErrors(pointStats: Array[(Int, (Array[Int], Double, Vector[Double], Long, Set[Int]))]) = {
		val erreurs = pointStats.groupBy(_._1).mapValues(_.map(_._2._2).sum).toArray //erreurs: (label, error)
		for ( er <- erreurs ) if( this.errors.size < er._1 ) this.errors(er._1) += er._2
  }

  // add new nodes --x3
  def addNewNodes(nbNodesToAdd: Int, kk: Int, dim: Int, alphaErr: Double) = {
		for (j <- 1 to nbNodesToAdd) {
		  // find the node with the largest error		
		  val q = this.errors.indexOf(this.errors.max)
		  
		  //val (errs, idxErr) = erreurs.zipWithIndex.sorted.unzip
		  val qNeighbors = this.edges(q).zipWithIndex.collect{ case (v, idx) if v == 1 => idx }
      
		  // find the neighbor f with the largest accumulated error 
      val f = this.errors.indexOf(this.errors.zipWithIndex.collect{ case (a, b) if qNeighbors.exists(_ == b) => a }.max)

      // Add the new node half-way between nodes q and f: nodes = [nodes .5*(nodes(:,q)+nodes(:,f))]
      val node1 = this.nodes(q).protoPartNum + this.nodes(f).protoPartNum
      node1 :*= 0.5 
      val protoNode1 = new Prototype(node1, Set(), this.nodes.length + 1) 
      this.nodes += protoNode1		

 		  this.clusterWeights += 0
 		  
      // Remove the original edge between q and f.
      this.edges(q)(f) = 0
		  this.edges(f)(q) = 0    
      this.ages(q)(f) = Double.NaN  
		  this.ages(f)(q) = Double.NaN    
      // insert edges connecting the new unit r with units q and f.
      val r = this.edges.size
      this.edges = this.addElementLast(0, this.edges)
      this.edges(q)(r) = 1
		  this.edges(r)(q) = 1
      this.edges(f)(r) = 1  
		  this.edges(r)(f) = 1
    
    	this.ages = this.addElementLast(Double.NaN, this.ages)
    	this.ages(q)(r) = 0 
		  this.ages(r)(q) = 0
    	this.ages(f)(r) = 0 
		  this.ages(r)(f) = 0
    
      this errors(q) = this.errors(q) * alphaErr
      this errors(f) = this.errors(f) * alphaErr 
      this.errors += (this.errors(q) + this.errors(f))
        
		}    
  }
  
  // fading function
  def fading(minWeight: Double) = {
    if (nodes.size != clusterWeights.size) throw new IllegalStateException("The size of nodes and weights must be equal !")

    val weightsWithIndex = this.clusterWeights.view.zipWithIndex
    val (minW, smallest) = weightsWithIndex.minBy(_._1)
    if (minW < minWeight) {
      // save this node as an outdated node
      this.outdatedNodes += this.nodes(smallest)
      
      // delete
      this.edges = this.removeLineColInt(smallest, this.edges)
      this.ages = this.removeLineCol(smallest, this.ages)        
      this.clusterWeights.remove(smallest)
      this.errors.remove(smallest)
      this.nodes.remove(smallest)
    }
  }
  
  
  // temperature
  def temperature() = 0.3

  // the K-Neighborhood function
  def kNeighbor(neuron1: Prototype, neuron2: Prototype, t: Double): Double = exp( - 1 / t ) //the vicinity !
  
  // the K-Neighborhood function
  def kNeighborSOM(neuron1: Prototype, neuron2: Prototype, t: Double): Double = exp( - (squaredDistance(neuron1.protoPartNum, neuron2.protoPartNum)) / t )
  
  // add a new line and a new column containing each an array of element e 
  def addElementLast(e: Double, a: mutable.ArrayBuffer[mutable.ArrayBuffer[Double]]) = {
    var b = a
      if ( a.isEmpty ) b += mutable.ArrayBuffer.fill(1)(e)
      else {
        b += mutable.ArrayBuffer.fill(a(0).size)(e)
        b.foreach(_ += e)
        b
      }        
    }
  def addElementLast(e: Int, a: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]]) = {
    var b = a
    if (a.isEmpty) b.+=(mutable.ArrayBuffer.fill(1)(e))
    else {
      b.+=(mutable.ArrayBuffer.fill(a(0).size)(e))
      b.foreach { x => x.+=(e) }
      b
    }        
  }
  def addElementLastMajGen[T: Numeric](e: T, a: mutable.ArrayBuffer[mutable.ArrayBuffer[T]]) = {
    if ( a.isEmpty ) a :+ mutable.ArrayBuffer.fill(1)(e)
    else (a :+ mutable.ArrayBuffer.fill(a.head.size)(e)).map(_ += e)
  }
  // remove the ith line and the ith column 
  def removeLineCol(i: Int, a: mutable.ArrayBuffer[mutable.ArrayBuffer[Double]]): mutable.ArrayBuffer[mutable.ArrayBuffer[Double]] = {
    var b = a
        b.remove(i)
        b.foreach(_.remove(i))
        b
  }
  def removeLineColInt(i: Int, a: mutable.ArrayBuffer[mutable.ArrayBuffer[Int]]): mutable.ArrayBuffer[mutable.ArrayBuffer[Int]] = {
    var b = a
        b.remove(i)
        b.foreach(_.remove(i))
        b
  }
  // remove the ith line and the ith column 
  def removeLineColMajGen[T: Numeric](i: Int, a: mutable.ArrayBuffer[mutable.ArrayBuffer[T]]): mutable.ArrayBuffer[mutable.ArrayBuffer[T]] = {
    val b = a
    b.remove(i)
    b.foreach(_.remove(i))
    b
  }  
  //	res = a x b 
  def scal(a: Double, b: Array[Double]): Array[Double] = b.zipWithIndex.map{ case (x, y) => a * x } 

  def scal(a: Double, b: mutable.ArrayBuffer[Double]): mutable.ArrayBuffer[Double] = b.zipWithIndex.map{ case (x, y) => a * x }

  def scal(a: Double, b: Vector[Double]): Vector[Double] = Vector(scal(a, b.toArray))

  def addPairwise[N: ClassTag](a: Array[N], b: Array[N])(implicit num: Numeric[N]): Array[N] = (a zip b).map{ case (x, y) => num.plus(x, y) }.toArray

  def addPairwise(a: mutable.ArrayBuffer[Double], b: mutable.ArrayBuffer[Double]): mutable.ArrayBuffer[Double] = (a zip b).map{ case (x, y) => x + y }

  def axpy(a: Double, x: Array[Double], y: Array[Double]) = addPairwise(scal(a, x), y)

  def axpy(a: Double, x: Vector[Double], y: Vector[Double]) = {
    x :*= a
    x + y
  }

  def axpy(a: Int, x: Vector[Int], y: Vector[Int]) = {
    x :*= a
    x + y
  }  

  
  // test if two vectors are equals
  def areQuasiEqual(a: Vector[Double], b: Vector[Double], epsilon: Double = 1e-10):Boolean = {
    var ok = true
    if( a.length != b.length ) ok = false
    else {
      var i = 0
      while( ok & i < a.length ) {
        if (math.abs(a(i)-b(i)) > epsilon) ok = false
        i += 1
      }
    }
    ok
  } 
  
  
  // toString methods
  override def toString = this.nodes.toArray.deep.mkString("\n")
  def toStringIds = this.nodes.map(_.toStringIds)
  def toStringOutdatedIds = this.outdatedNodes.map(_.toStringIds)  
  def toStringProto = this.nodes.map(_.toStringProto)
  def toStringCard = this.nodes.map(_.toStringCard)
  def toStringAss = this.nodes.map(_.toStringAss)
  def toStringOutdatedAss = this.outdatedNodes.map(_.toStringAss)
  def toStringOutdatedProto = this.outdatedNodes.map(_.toStringProto)
  def toStringIdNode = this.nodes.map(_.toStringId)
  def toStringIdOutdated = this.outdatedNodes.map(_.toStringId)

}
