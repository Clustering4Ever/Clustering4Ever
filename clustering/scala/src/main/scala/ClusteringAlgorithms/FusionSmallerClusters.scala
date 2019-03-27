package org.clustering4ever.clustering.centersfusionner.scala

import scala.util.Random
import org.clustering4ever.util.ClusterBasicOperations
import scala.collection.{mutable, immutable, GenSeq, GenMap}
import org.clustering4ever.math.distances.{ContinuousDistance, BinaryDistance}
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.clustering.ClusteringSharedTypes
import org.clustering4ever.vectors.GVector/**
 *
 */
trait FusionSmallerClusters extends ClusteringSharedTypes {
	/**
	 *
	 */
	final type NewClusterID = Int
	/**
	 *
	 */	
	final type ID = Int
	/**
	 *
	 */	
	private[this] final def selectSmallerClusterIDs[Cardinality](clusterCardinalities: scala.collection.Map[ClusterID, Cardinality], cmin: Int)(implicit num: Numeric[Cardinality]) = {
		clusterCardinalities.filter{ case (clusterID, cardinality) => num.toLong(cardinality) < cmin }.keys.toBuffer
	}
	/**
	 *
	 */	
	private[this] final def selectSmallerCluster[Cardinality, V <: GVector[V]](toGatherCentroids: GenSeq[(ID, NewClusterID, V, Cardinality, ClusterID)], cmin: Int)(implicit num: Numeric[Cardinality]) = {
		mutable.ArrayBuffer(toGatherCentroids.filter{ case (_, _, _, cardinality, _) => num.toLong(cardinality) < cmin }.seq:_*)
	}
	/**
	 *
	 */	
	private[this] final def obtainToGatherCentroids[Cardinality, V <: GVector[V]](centroids: GenSeq[(Int, V)], clusterCardinalities: scala.collection.Map[ClusterID, Cardinality]) = {
		centroids.zipWithIndex.map{ case ((clusterID, vector), id) => (id, clusterID, vector, clusterCardinalities(clusterID), clusterID) }
	}
	/**
	 *
	 */	
	private[this] final def settingUpFusionAlgorithm[Cardinality: Numeric, V <: GVector[V]](centroids: GenSeq[(Int, V)], clusterCardinalities: scala.collection.Map[ClusterID, Cardinality], cmin: Int) = {
		val clusterIDsOfSmallerOne = selectSmallerClusterIDs[Cardinality](clusterCardinalities, cmin)
		val toGatherCentroids = obtainToGatherCentroids(centroids, clusterCardinalities)
		val littleClusters = selectSmallerCluster(toGatherCentroids, cmin)
		(clusterIDsOfSmallerOne, toGatherCentroids.toArray, littleClusters)
	}
	/**
	 *
	 */	
	private[this] final def findClosestCentroidWithAnotherClusterID[Cardinality, V <: GVector[V]](sortedClosestCentroid: Seq[(Int, NewClusterID, V, Cardinality, Int)], currentClusterID: Int) = {
		sortedClosestCentroid.find{ case (_, newClusterID, _, _, _) => newClusterID != currentClusterID }.get
	}
	/**
	 *
	 */	
	private[this] final def filterOthersCentroidWithSameClusterID[Cardinality, V <: GVector[V]](sortedClosestCentroid: Seq[(Int, NewClusterID, V, Cardinality, Int)], closestClusterID: Int) = {
		sortedClosestCentroid.filter{ case (_, newClusterID, _, _, _) => newClusterID == closestClusterID }
	}
	/**
	 *
	 */	
	private[this] final def settingUpBuffers[Cardinality, V <: GVector[V]](
		sortedClosestCentroid: Seq[(Int, Int, V, Cardinality, Int)],
		sizeCurrent: Cardinality,
		origVector: V,
		currentClusterID: Int,
		idx: Int
	)(implicit num: Numeric[Cardinality]) =	{
		val (idx2, closestClusterID, _, closestClusterSize, _) = findClosestCentroidWithAnotherClusterID(sortedClosestCentroid, currentClusterID)
		val othersMicroCentroids = filterOthersCentroidWithSameClusterID(sortedClosestCentroid, closestClusterID)
		val totSize = num.plus(sizeCurrent, closestClusterSize)
		val lookForNN = mutable.ListBuffer(origVector) ++= othersMicroCentroids.map(_._3)
		val oldClusterIDs = mutable.HashSet(currentClusterID, closestClusterID)
		val idxToReplace = mutable.HashSet(idx, idx2)

		(
			totSize,
			lookForNN,
			oldClusterIDs,
			idxToReplace,
			closestClusterID
		)	
	}
	/**
	 *
	 */	
	private[this] final def obtainIDsOfCentroidToUpdate[Cardinality: Numeric, V <: GVector[V]](toGatherCentroids: Array[(ID, NewClusterID, V, Cardinality, ClusterID)], oldClusterIDs: mutable.HashSet[Int]) = {
		toGatherCentroids.collect{ case (id, newClusterID, _, _, _) if oldClusterIDs.contains(newClusterID) => id }
	}
	/**
	 * @param centroids: GenSeq of centroids with their ID and vector prototype
	 * @param clusterCardinalities
	 * Fusion clusters with cardinality under cmin to their closest aggregate of cluster looking on each cluster prototype to make its decision
	 **/
	final def fusionSmallerClusters[Cardinality, V <: GVector[V], D <: Distance[V]](centroids: GenSeq[(ClusterID, V)], clusterCardinalities: scala.collection.Map[ClusterID, Cardinality], cmin: Int, metric: D)(implicit num: Numeric[Cardinality]): (immutable.Map[Int, Int], immutable.Map[Int, Cardinality]) = {

		val numericCMIN = num.fromInt(cmin)

		val (clusterIDsOfSmallerOne, toGatherCentroids, littleClusters) = settingUpFusionAlgorithm(centroids, clusterCardinalities, cmin)

		while(!clusterIDsOfSmallerOne.isEmpty) {
			val (idx, currentClusterID, origVector, sizeCurrent, _) = littleClusters(Random.nextInt(littleClusters.size)) 
			val sortedClosestCentroid: Array[(Int, Int, V, Cardinality, Int)] = toGatherCentroids.sortBy{ case (_, _, vector, _, _) => metric.d(vector, origVector) }.map{ case (id, newClusterID, centroid, cardinality, oldClusterID) => (id, newClusterID, centroid, cardinality, oldClusterID) }

			val (totSizeOut, lookForNN, oldClusterIDs, idxToReplace, closestClusterID) = settingUpBuffers(sortedClosestCentroid, sizeCurrent, origVector, currentClusterID, idx)
			var totSize = totSizeOut
			while(num.lt(totSize, numericCMIN)) {
			  	val (idxK, vectorK, clusterIDK, cardinalityK) =
			  	{
				    val (idxK, clusterIDK, vectorK, cardinalityK, _) = lookForNN.map{ v =>
				    	val (id, newClusterID, vector, cardinality, _) = toGatherCentroids.filter{ case (_, newClusterID, _, _, _) => ! oldClusterIDs.contains(newClusterID) }
				    		.minBy{ case (_, _, vector, _, _) => metric.d(vector, origVector) }
				    	(id, newClusterID, vector, cardinality, metric.d(vector, origVector))
				    }.minBy(_._5)
				    (idxK, vectorK, clusterIDK, cardinalityK)
			  	}

			    lookForNN ++= sortedClosestCentroid.collect{ case (_, newClusterID, centroid, _, _) if newClusterID == clusterIDK => centroid }
			    oldClusterIDs += clusterIDK
			    idxToReplace += idxK
			    totSize = num.plus(totSize, cardinalityK)
			}

			idxToReplace ++= obtainIDsOfCentroidToUpdate(toGatherCentroids, oldClusterIDs)

			idxToReplace.foreach{ idxR =>
				val (idR, _, vectorR, cardinalityR, originalClusterIDR) = toGatherCentroids(idxR)
				if(num.gt(totSize, numericCMIN)) {
					clusterIDsOfSmallerOne -= originalClusterIDR
					littleClusters -= ((idR, originalClusterIDR, vectorR, cardinalityR, originalClusterIDR))
				}
				toGatherCentroids(idxR) = (idxR, closestClusterID, vectorR, totSize, originalClusterIDR)
			}
		}

		val newClustersIdsMapping = toGatherCentroids.map(_._2).distinct.zipWithIndex.toMap
		val newClusterIDByOldOne = toGatherCentroids.map{ case (_, newClusterID, centroid, cardinality, oldClusterID) => (oldClusterID, newClustersIdsMapping(newClusterID)) }.toMap
		val cardinalitiesNewClusters = toGatherCentroids.map{ case (_, newClusterID, centroid, cardinality, oldClusterID) => (newClustersIdsMapping(newClusterID), cardinality) }.toMap
		(newClusterIDByOldOne, cardinalitiesNewClusters)
	}
	/**
	 *
	 */
	// final def obtainDataToFusionLittleCluster[N: Numeric](clusterizedDS: GenSeq[(Int, (Int, Seq[N]))]): (GenMap[Int, Seq[N]], GenMap[Int,Int]) = {
	// 	val isRealVectorElseBinary = clusterizedDS.head._2._2.isInstanceOf[Double]
	// 	val gatheredData = clusterizedDS.groupBy{ case (clusterID, _) => clusterID }
	// 	val clusterCardinalities = gatheredData.map{ case (clusterID, aggregates) => (clusterID, aggregates.size) }
	// 	val centroids = gatheredData.map{ case (clusterID, aggregates) =>
	// 		(
	// 			clusterID,
	// 			{
	// 				val vectors = aggregates.map{ case (_, (_, vector)) => vector }
	// 				// ClusterBasicOperations.obtainCenter(vectors, metric)
	// 				(if(isRealVectorElseBinary) ClusterBasicOperations.obtainMean(vectors.asInstanceOf[GenSeq[Seq[Double]]]) else ClusterBasicOperations.obtainMedian(vectors.asInstanceOf[GenSeq[Seq[Int]]])).asInstanceOf[Seq[N]]
	// 			}
	// 		)
	// 	}
	// 	(centroids, clusterCardinalities)
	// }
	/**
	 *
	 */
	final def fusionLittleClustersIn[V <: GVector[V], D <: Distance[V]](labelizedData: GenSeq[(Int, (Int, V))], cmin: Int, metric: D): GenSeq[(Int, (Int, V))] = {
		if(cmin == 0) labelizedData
		else {
			val clusters = labelizedData.groupBy{ case (clusterID, _) => clusterID }
			val clustersCardinalities = clusters.map{ case (clusterID, aggregates) => (clusterID, aggregates.size.toLong) }.seq.toMap
			val centroids = clusters.map{ case (clusterID, aggregates) =>
				(
					clusterID,
					ClusterBasicOperations.obtainCenter(aggregates.map{ case (_, (_, vector)) => vector }, metric)
				)
			}.toSeq.par
			val (newClusteringMapping, _) = fusionSmallerClusters(centroids, clustersCardinalities, cmin, metric)
			val finalLabelizedData = labelizedData.map{ case (clusterID, (id, vector)) => (newClusteringMapping(clusterID), (id, vector)) }
			finalLabelizedData
		}
	}
}
/**
 *
 */
object FusionSmallerClusters extends FusionSmallerClusters {
	/**
	 * Fusion clusters with cardinality under cmin to their closest aggregate of cluster looking on each cluster prototype to make its decision
	 */
	final def fusionSmallerCluster[Cardinality: Numeric, V <: GVector[V], D <: Distance[V]](centroids: GenSeq[(ClusterID, V)], clusterCardinalities: scala.collection.Map[ClusterID, Cardinality], cmin: Int, metric: D): (immutable.Map[Int, Int], immutable.Map[Int, Cardinality]) = {
		fusionSmallerClusters(centroids, clusterCardinalities, cmin, metric)
	}
	/**
	 * Fusion clusters with cardinality under cmin to their closest aggregate of cluster looking on each cluster prototype to make its decision
	 */
	final def fusionLittleClusters[V <: GVector[V], D <: Distance[V]](labelizedData: GenSeq[(ClusterID, (Int, V))], cmin: Int, metric: D): GenSeq[(Int, (Int, V))] = {
		fusionLittleClustersIn(labelizedData, cmin, metric)
	}
}