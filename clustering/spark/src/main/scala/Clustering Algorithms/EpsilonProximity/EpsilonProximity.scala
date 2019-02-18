package org.clustering4ever.clustering.epsilonproximity.rdd
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import Numeric._
import scala.util.Random
import scala.collection.{mutable, immutable, GenSeq}
import scala.math.{pow, sqrt, max, min}
import scala.util.Try
import scala.reflect.ClassTag
import scala.language.higherKinds
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.{ContinuousDistance, Distance}
import org.clustering4ever.util.SumVectors
import org.clustering4ever.clustering.centersfusionner.scala.FusionSmallerClusters
import org.clustering4ever.clustering.epsilonproximity.scala.{EpsilonProximityScalar => EpsilonProximityScalarLocal}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.sparktools.UtilSpark
import org.clustering4ever.util.SimilarityMatrix
import org.clustering4ever.hashing.{Hashing, HashingScalar}
import org.clustering4ever.util.VectorsAddOperationsImplicits._
import org.clustering4ever.vectors.{GVector, ScalarVector}
import org.clustering4ever.clustering.rdd.{ClusteringModelDistributed, ClusteringAlgorithmDistributed}
/**
 * @tparam V the working vector nature
 * @tparam D the distance
 * @tparam Hash the hashing function
 */
trait EpsilonProximityModelAncestor[V <: GVector[V], D <: Distance[V], Hash <: Hashing[V]] extends ClusteringModelDistributed[V] {
	/**
	 * The clusterized dataset in its basic form RDD[ID, (Vector, ClusterID)]
	 */
	val clusterizedVectorsSortedByID: RDD[(Long, (V, Int))]
	/**
	 * The metric used in this algorithm/model
	 */
	val metric: D
	/**
	 * The number of generated clusters
	 */
	val clustersNumber: Int
	/**
	 * The hashcode of input dataset to prevent missuse of some methods
	 */
	protected val inputDataHashCode: Int
}
/**
 *
 * @tparam V the working vector nature
 * @tparam D the distance
 * @tparam Hash the hashing function
 * @param clusterizedVectorsSortedByID
 * @param metric
 * @param clustersNumber
 * @param inputDataHashCode
 */
final case class EpsilonProximityModelScalar[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X], Hash <: HashingScalar[V]](final val clusterizedVectorsSortedByID: RDD[(Long, (ScalarVector[V], Int))], final val metric: D[V], final val clustersNumber: Int, protected final val inputDataHashCode: Int) extends EpsilonProximityModelAncestor[ScalarVector[V], D[V], Hash] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximityScalar

	protected[clustering] final def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, ScalarVector[V]]])(implicit ct: ClassTag[Cz[O, ScalarVector[V]]]): RDD[Cz[O, ScalarVector[V]]] = {
		data.map( cz => (cz.id, cz) ).repartitionAndSortWithinPartitions(new HashPartitioner(data.getNumPartitions)).zip(clusterizedVectorsSortedByID).map{ case ((_, cz), (_, (_, clusterID))) =>  cz.addClusterIDs(clusterID) }
	}
	/**
	 * This method work only with input dataset which generate this model, please use others method for new set of points 
	 *
	 * @return the clusterized dataset
	 */
	final def obtainInputDataClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, ScalarVector[V]]])(implicit ct: ClassTag[Cz[O, ScalarVector[V]]]): RDD[Cz[O, ScalarVector[V]]] = {
		require(inputDataHashCode == data.hashCode, println("This method work only with input dataset which generate this model, please use others method for predict new set of points"))
		obtainClustering(data)
	}

}
/**
 *
 */
trait EpsilonProximityAncestor[V <: GVector[V], D <: Distance[V], Hash <: Hashing[V], CM <: EpsilonProximityModelAncestor[V, D, Hash]] extends ClusteringAlgorithmDistributed[V, CM] {
	/**
	 * Desired value of epsilon
	 *
	 * Two option for epsilon choice
	 *  - eps:value with value a numeric value ie eps:8.8, eps:1 which run the algorithm with the defined epsilon
	 *  - knn:intValue ie knn:20 which will define epsilon as the mean of the 20 nearest neighbors of every points
	 */
	val epsilon: String
	/**
	 * Recommended value : "bydot:1"
	 */
	val fusionClustersMethod: String
	/**
	 *
	 */
	val metric: D
	/**
	 *
	 */
	val bucketNumber: Int
	/**
	 *
	 */
	val hasher: Hash
	/**
	 *
	 */
	val cmin: Int
	/**
	 *
	 */
	val bucketLayers: Int
	/**
	 *
	 */
	val storageLevel: StorageLevel
	/**
	 *
	 */
	val divisionFactor: Int
}
/**
 * @tparam V
 * @tparam D
 * @tparam Hash
 * @param epsilon main parameter which define at which distance points are considered in the same cluster
 * @param fusionClustersMethod
 * @param metric
 * @param bucketNumber
 * @param hasher
 * @param cmin
 * @param bucketLayers
 * @param storageLevel
 * @param divisionFactor
 */
final case class EpsilonProximityScalar[
	V <: Seq[Double],
	D[X <: Seq[Double]] <: ContinuousDistance[X],
	Hash <: HashingScalar[V]
](
	final val epsilon: String,
	final val fusionClustersMethod: String,
	final val metric: D[V],
	final val bucketNumber: Int,
	final val hasher: Hash,
	final val cmin: Int = 0,
	final val bucketLayers: Int = 1,
	final val storageLevel: StorageLevel = StorageLevel.MEMORY_ONLY,
	final val divisionFactor: Int = 1
) extends EpsilonProximityAncestor[ScalarVector[V], D[V], Hash, EpsilonProximityModelScalar[V, D, Hash]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximityScalar

	final type IndexPartition = Int
	final type PartitionIndex = Int
	final type IndexPartitionOriginal = Int
	final type EpsilonPerBucket = Double
	final type FusionableClusterizedData[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]] = List[(IndexPartition, (IndexPartitionOriginal, ClusterID, mutable.ArrayBuffer[Cz[O, ScalarVector[V]]]))]

	private[this] def applyLshAndDefinedEpsilon[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, ScalarVector[V]]]): (RDD[Cz[O, ScalarVector[V]]], String) = {
		val nbDim = data.first.v.vector.size

		val Array(epsilonNature, epsilonArg) = epsilon.split(":")

		val lshizedRDD = data.sortBy( cz => hasher.hf(cz.v), ascending = true, bucketNumber ).persist(storageLevel)

		val computedEpsilon = if(epsilonNature == "eps") epsilon
			else {
				val k = epsilonArg.toInt

				def definedKNNEpsilon = {

					def epsilonKnnDistanceValue(simMat: scala.collection.GenMap[Long, GenSeq[(Cz[O, ScalarVector[V]], Double)]]) = {
						simMat.map{ case (_, v) => v.map(_._2).apply(k + 1) }.sum / simMat.size
					}

				    val localityRDD = UtilSpark.generateDataLocalityOnHashsedDS(lshizedRDD, bucketNumber, bucketLayers)

				    val sumEpsilon = localityRDD.mapPartitions{ it =>
				    	// val dataIn = it.map{ case (_, (cz, _, _)) => (cz.id, cz) }.toBuffer
				    	val dataIn = it.map{ case (_, (cz, _, _)) => cz }.toBuffer
						val simMat = SimilarityMatrix.sortedSimilarityMatrixWithVector(dataIn, metric)
						val eps = epsilonKnnDistanceValue(simMat)
						Iterator(eps)
				    }.sum

				    sumEpsilon / localityRDD.getNumPartitions
				}

				"eps:" + definedKNNEpsilon
			}

		
		data.unpersist(false)

		(lshizedRDD, computedEpsilon)
	}

	private[this] final def applyClusteringPerBucket[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](lshizedRDD: RDD[Cz[O, ScalarVector[V]]], computedEpsilon: String): RDD[(immutable.Iterable[(Int, Int, mutable.ArrayBuffer[Cz[O, ScalarVector[V]]])], Double)] = {
		lshizedRDD.mapPartitionsWithIndex{ (idxPart, it) =>

		    val data = it.toBuffer.sortBy{ cz: Cz[O, ScalarVector[V]] => cz.id }

		    val epsilonLocalModel = EpsilonProximityScalarLocal(computedEpsilon, metric).fit(data)

		   	val labeledDS = epsilonLocalModel.obtainClustering(data).groupBy(_.clusterIDs.last).map{ case (clusterID, cluster) => (idxPart, clusterID, mutable.ArrayBuffer(cluster.seq:_*)) }.seq

		    Iterator((labeledDS, epsilonLocalModel.epsilon))
	 	}
	}


	private[this] final def exchangeDataBetweenPartitions[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](clusterizedBDS: RDD[(immutable.Iterable[(PartitionIndex, ClusterID, mutable.ArrayBuffer[Cz[O, ScalarVector[V]]])], EpsilonPerBucket)]): RDD[(Int, (Int, Int, mutable.ArrayBuffer[Cz[O, ScalarVector[V]]]))] = {
		clusterizedBDS.mapPartitions{ it =>
		    it.flatMap(_._1.flatMap{ case (idxPart, idClust, rest) =>
	    		if(idxPart == bucketNumber - 1) List((idxPart, (idxPart, idClust, rest)))
	    		//else if( idxPart == 1 ) Seq((idxPart, (idxPart, idClust, rest)), (idxPart + 1, (idxPart, idClust, rest)), (idxPart - 1, (idxPart, idClust, rest)))
                else List((idxPart, (idxPart, idClust, rest)), (idxPart + 1, (idxPart, idClust, rest)))
            })
  		}.partitionBy(new HashPartitioner(bucketNumber))
	}

	private[this] final def fusion2partitionsOfClusters[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](epsilonAvg: Double, left: FusionableClusterizedData[O, Cz], right: FusionableClusterizedData[O, Cz]): mutable.HashSet[List[(Int, Int)]] = {
		val parsedMethodChoice = fusionClustersMethod.split(":")
		val theMethodChoice = parsedMethodChoice.head
		val continuityThreshold = Try(parsedMethodChoice.last.toInt).getOrElse(1)

    	def obtainNeighborsClusterFromStudiedOne(oneSide: FusionableClusterizedData[O, Cz], haveClustersCommonDots: mutable.ArrayBuffer[Cz[O, ScalarVector[V]]] => Boolean) = {	
        	oneSide.collect{ case (idxPart2, (idxPartOr2, idClust2, cluster2)) if(haveClustersCommonDots(cluster2)) => (idxPartOr2, idClust2) }
        	// for( (idxPart2, (idxPartOr2, idClust2, cluster2)) <- oneSide if( haveClustersCommonDots(cluster2) ) ) yield (idxPartOr2, idClust2)
    	}
		/**
		 * Fusion
		 * Worst case in O(n.m) with n and m cardinality of each cluster
		 */
		def fusionByDotsProximity(oneSide: FusionableClusterizedData[O, Cz], clusterToStudy: mutable.ArrayBuffer[Cz[O, ScalarVector[V]]]) = {
			def isThereEnoughCloseDotsBetweenTwoClusters(cluster2: mutable.ArrayBuffer[Cz[O, ScalarVector[V]]]) = {
				def incrementLimit(i: Int, j: Int, l: Int) = {
					val vector1 = clusterToStudy(i).v
					val vector2 = cluster2(j).v
					val euclideanD = metric.d(vector1, vector2)
					if(euclideanD <= epsilonAvg) l + 1 else l
				}

				@annotation.tailrec
				def go(i: Int, j: Int, l: Int): Int = {
					val incLim = incrementLimit(i, j, l)
					if(l < continuityThreshold) {
						if(i >= 0 && j > 0) go(i, j - 1, incLim)
						else if(i > 0 && j == 0) go(i - 1, cluster2.size - 1, incLim)
						else incLim
					}
					else l
				}

				val limit = go(clusterToStudy.size - 1, cluster2.size - 1, 0)

	        	limit >= continuityThreshold
        	}

        	obtainNeighborsClusterFromStudiedOne(oneSide, isThereEnoughCloseDotsBetweenTwoClusters)
		}

    	def obtainCommonClusters(oneSide: FusionableClusterizedData[O, Cz], clusterToStudy: mutable.ArrayBuffer[Cz[O, ScalarVector[V]]]): List[(IndexPartitionOriginal, ClusterID)] = {
	        val closeClusters = theMethodChoice match {
	        	case "bydot" => fusionByDotsProximity(oneSide, clusterToStudy)
	        	case _ => fusionByDotsProximity(oneSide, clusterToStudy)
	        }
	        closeClusters
	    }

		val sameCluster: List[List[(IndexPartitionOriginal, ClusterID)]] = left.map{ case (idxPart1, (idxPartOr1, idClust1, cluster1)) =>
			obtainCommonClusters(right, cluster1) :+ ((idxPartOr1, idClust1))
		}

		val newKeys: immutable.Map[(Int, Int), Int] = sameCluster.flatten.zipWithIndex.toMap
		val reverseKeys: immutable.Map[Int, (Int, Int)] = newKeys.map(_.swap)
		val newClusters: List[List[Int]] = sameCluster.map( gatheredCluster => gatheredCluster.map(newKeys) )
		val (nodes, neighbours) = GatherClustersWithSharedDots.generateNodesAndPairsNew(newClusters)
		val finalClusters = mutable.HashSet(GatherClustersWithSharedDots.reduceByGatheringClusters(nodes, neighbours).map(_.map(reverseKeys)):_*)

	    finalClusters
	}

	private[this] final def separateTwoPartitions[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](fusionOf2Buckets: List[(IndexPartition, (IndexPartitionOriginal, ClusterID, mutable.ArrayBuffer[Cz[O, ScalarVector[V]]]))]) = {
	    val oneOfPartIdx = fusionOf2Buckets.head._2._1
		fusionOf2Buckets.partition{ case (_, (idxPartOr, _, _)) => idxPartOr == oneOfPartIdx }
	}
	

	private[this] final def fusionClustersInsideEachPartition[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](epsilonAvg: Double, eachBucketPairFusionedIntoOneRDD: RDD[(IndexPartition, (IndexPartitionOriginal, ClusterID, mutable.ArrayBuffer[Cz[O, ScalarVector[V]]]))]) = {
		eachBucketPairFusionedIntoOneRDD.mapPartitions{ it =>
			val data =  it.toList
			val (left, right) = separateTwoPartitions(data)
			val setGatheredClusterAndPartIdsFinal = fusion2partitionsOfClusters(epsilonAvg, left, right)

	    	val lonelyCluster: mutable.Buffer[List[(Int, Int)]] = data.collect{ case (_, (idxPartOr, idClust, _)) if !setGatheredClusterAndPartIdsFinal.exists(_.contains((idxPartOr, idClust))) => List((idxPartOr, idClust)) }.toBuffer

			Iterator((lonelyCluster ++ setGatheredClusterAndPartIdsFinal).toList)
	    }		
	}

	private[this] final def fusionBuckets[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](lshizedRDD: RDD[Cz[O, ScalarVector[V]]], computedEpsilon: String) = {
		val clusterizedBDS = applyClusteringPerBucket(lshizedRDD, computedEpsilon).persist(storageLevel)
		lshizedRDD.unpersist(false)
		val epsilonAvg = clusterizedBDS.map(_._2).sum / bucketNumber
		val eachBucketPairFusionedIntoOneRDD = exchangeDataBetweenPartitions(clusterizedBDS).persist(storageLevel)
		val linkedClusterOverRDDPartitions = fusionClustersInsideEachPartition(epsilonAvg, eachBucketPairFusionedIntoOneRDD)
		(linkedClusterOverRDDPartitions, eachBucketPairFusionedIntoOneRDD)
	}

	private[this] final def giveNewLabelToEachCluster(linkedClusterOverRDDPartitions: RDD[List[List[(Int, Int)]]]): Map[(Int, Int), Int] = {
		linkedClusterOverRDDPartitions.flatMap(_.flatten).distinct.collect.zipWithIndex.toMap
	}


	private[this] final def merge2Maps(m1: immutable.HashMap[Int, immutable.HashSet[Int]], m2: immutable.HashMap[Int, immutable.HashSet[Int]]): immutable.HashMap[Int, immutable.HashSet[Int]] = {	
		val m1k = m1.keys.toSet
		val m2k = m2.keys.toSet
		val commonKeys = m1k & m2k
		val um1k = m1k &~ commonKeys
		val um2k = m2k &~ commonKeys
		/*
		val m3 = mutable.HashMap(commonKeys.map( k => (k, m1(k) ++ m2(k)) ).toSeq:_*)
		m3 ++= um1k.map( k => (k, m1(k)) )
		m3 ++= um2k.map( k => (k, m2(k)) )
		m3
		*/
		immutable.HashMap(commonKeys.toSeq.map( k => (k, m1(k) ++ m2(k)) ) ++ um1k.map( k => (k, m1(k)) ) ++ um2k.map( k => (k, m2(k)) ):_*)
		// Spark issue
		// m1.merged(m2){ case ((k, v1), (_, v2)) => (k, v1 ++ v2) }
	}

	private[this] final def applyEpsilonClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, ScalarVector[V]]])(implicit ct: ClassTag[Cz[O, ScalarVector[V]]]): EpsilonProximityModelScalar[V, D, Hash] = {
		data.persist(storageLevel)
		val (lshizedRDD, computedEpsilon) = applyLshAndDefinedEpsilon(data)
		val (linkedClusterOverRDDPartitions, eachBucketPairFusionedIntoOneRDD) = fusionBuckets(lshizedRDD, computedEpsilon)
		linkedClusterOverRDDPartitions.persist(storageLevel)
		val newClusterIdByPairId = giveNewLabelToEachCluster(linkedClusterOverRDDPartitions)
		val linkedClusterOverRDDPartitions2 = linkedClusterOverRDDPartitions.map(_.map(_.map(newClusterIdByPairId(_))))
		val (nodes2, neighbours2) = if(divisionFactor <= 1) linkedClusterOverRDDPartitions2.map(GatherClustersWithSharedDots.generateNodesAndPairsNew(_))
			.reduce{ case ((nodes1, neighbours1), (nodes2, neighbours2)) => (nodes1 ++ nodes2, merge2Maps(neighbours1, neighbours2)) }
		// Experimental
			else {
				linkedClusterOverRDDPartitions2.map(GatherClustersWithSharedDots.generateNodesAndPairsNew(_))
					.mapPartitionsWithIndex( (idx, it) => it.map( a => (idx / divisionFactor, a)) )
					.reduceByKey{ case ((nodes1, neighbours1), (nodes2, neighbours2)) => (nodes1 ++ nodes2, merge2Maps(neighbours1, neighbours2)) }
					.map{ case (_, (nodes, neighbours)) => GatherClustersWithSharedDots.generateNodesAndPairsNew(GatherClustersWithSharedDots.reduceByGatheringClusters(nodes, neighbours)) }
					.reduce{ case ((nodes1, neighbours1), (nodes2, neighbours2)) => (nodes1 ++ nodes2, merge2Maps(neighbours1, neighbours2)) }
			}

		val seqOfClusters = GatherClustersWithSharedDots.reduceByGatheringClusters(nodes2, neighbours2)
		val finalClusteringIdByOldOne: immutable.HashMap[Int, Int] = immutable.HashMap(seqOfClusters.zipWithIndex.flatMap{ case (cluster, idxC) => cluster.map((_, idxC)) }:_*)

		val preFinalClustering = eachBucketPairFusionedIntoOneRDD.flatMap{ case (_, (idxPartOr, idClust, rest)) => rest.map(_.addClusterIDs(finalClusteringIdByOldOne(newClusterIdByPairId((idxPartOr, idClust))))) }.distinct.persist(storageLevel)
		val clusterIDsMapping = preFinalClustering.map(_.clusterIDs.last).distinct.collect.zipWithIndex.toMap
		val finalClustering = preFinalClustering.map( cz => (cz.id, (cz.v, clusterIDsMapping(cz.clusterIDs.last))) ).repartitionAndSortWithinPartitions(new HashPartitioner(data.getNumPartitions))
		val clusterNumber = clusterIDsMapping.size
		EpsilonProximityModelScalar(finalClustering, metric, clusterNumber, data.hashCode)
	}

	private[this] final def applyEpsilonClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, ScalarVector[V]]], cmin: Int)(implicit ct: ClassTag[Cz[O, ScalarVector[V]]]): EpsilonProximityModelScalar[V, D, Hash] = {
		
		val model = applyEpsilonClustering(data)
		val clusterizedRDD = model.obtainClustering(data).persist(storageLevel)
		val numElemByCluster = clusterizedRDD.map( cz => (cz.clusterIDs.last, 1) ).countByKey
		val centroids = clusterizedRDD.map( cz => (cz.clusterIDs.last, cz.v) )
			.reduceByKeyLocally(SumVectors.sumVectors(_, _))
			.map{ case (clusterID, sVector) => (clusterID, ScalarVector(sVector.vector.map(_ / numElemByCluster(clusterID)).asInstanceOf[V])) }
			.toSeq

		val (newClusterIDByOldOne, cardinalitiesNewClusters) = FusionSmallerClusters.fusionSmallerCluster(centroids, numElemByCluster, cmin, metric)
		val finalRDD = clusterizedRDD.map( cz => (cz.id, (cz.v, newClusterIDByOldOne(cz.clusterIDs.last))) ).repartitionAndSortWithinPartitions(new HashPartitioner(data.getNumPartitions))
		val newClusterNumber = newClusterIDByOldOne.map(_._2).toSet.size
		EpsilonProximityModelScalar(finalRDD, metric, newClusterNumber, data.hashCode)
	}

	final def fit[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, ScalarVector[V]]])(implicit ct: ClassTag[Cz[O, ScalarVector[V]]]): EpsilonProximityModelScalar[V, D, Hash] = {
		if(cmin == 0) applyEpsilonClustering(data)
		else applyEpsilonClustering(data, cmin)
	}

}
/**
  *
  * The simplest way to launch Epsilon Clustering
  *
  *
  *
  **/
object EpsilonProximity
{
	final def fit[
		O,
		V <: Seq[Double],
		Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz],
		D[X <: Seq[Double]] <: ContinuousDistance[X],
		Hash <: HashingScalar[V]
	](
		data: RDD[Cz[O, ScalarVector[V]]],
		epsilon: String,
		metric: D[V],
		fusionClustersMethod: String,
		bucketNumber: Int,
		hasher: Hash,
		storageLevel: StorageLevel = StorageLevel.MEMORY_ONLY,
		bucketLayers: Int = 1,
		cmin: Int = 0,
		divisionFactor: Int = 0
	)(implicit ev: ClassTag[Cz[O, ScalarVector[V]]]): EpsilonProximityModelScalar[V, D, Hash] =
		EpsilonProximityScalar(epsilon, fusionClustersMethod, metric, bucketNumber, hasher, cmin, bucketLayers, storageLevel, divisionFactor).fit(data)

}