package org.clustering4ever.clustering.epsilonproximity.scala
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{immutable, mutable, GenSeq, GenMap}
import scala.collection.parallel
import scala.util.Try
import org.clustering4ever.util.SimilarityMatrix
import org.clustering4ever.clustering.ClusteringAlgorithmLocal
import org.clustering4ever.math.distances.{GenericDistance, Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.identifiables.IdentifiedRawObject
/**
 *
 */
trait EpsilonProximityAncestor[V <: GVector[V], D <: Distance[V], Model <: EpsilonProximityModelAncestor[V, D]] extends ClusteringAlgorithmLocal[V, Model] {
	/**
	 * Metric used in this algorithm
	 */
	val metric: D
	/**
	 * Desired value of epsilon
	 *
	 * Two option for epsilon choice
	 *  - eps:value with value a numeric value ie eps:8.8, eps:1 which run the algorithm with the defined epsilon
	 *  - knn:intValue ie knn:20 which will define epsilon as the mean of the 20 nearest neighbors of every points
	 */
	val epsilonChoice: String
	/**
	 *
	 */
	protected final def preModel[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]) = {

		def epsilonKnnEstimation(k: Int, simMat: scala.collection.Map[Long, Seq[(Cz[O, V], Double)]]) = simMat.map{ case (_, v) => v.map(_._2).apply(k + 1) }.sum / simMat.size

		val simMat = mutable.HashMap(SimilarityMatrix.sortedSimilarityMatrixWithVector(data.seq, metric).toSeq.seq:_*)
		val epsilon = epsilonChoice match {
            case x1 if(x1.contains("knn:")) => epsilonKnnEstimation(epsilonChoice.split(":").last.toInt, simMat)
            case x2 if(x2.contains("eps:")) => epsilonChoice.split(":").last.toDouble
            //case x3 if( epsilonChoice.contains("perc:") ) => epsilonPercEstimation(epsilonChoice.split(":").last.toInt, simMat)
            //case _ => epsilonPercEstimation(10, simMat)
		}
		val clusters = mutable.HashMap.empty[ClusterID, mutable.HashSet[Cz[O, V]]]
		var clusterID = 0
		val (startedCz, _) = simMat.head._2.head
		val toVisit: Option[mutable.HashSet[Cz[O, V]]] = Some(mutable.HashSet(startedCz))
		clusters += ((clusterID, toVisit.get))

		def getClosestDots(id: Long): Option[Seq[Cz[O, V]]] = simMat.get(id).map(_.collect{ case (cz, dist) if( dist <= epsilon ) => cz })

		@annotation.tailrec
		def feedClusters(toVisit: Option[mutable.HashSet[Cz[O, V]]]): Unit = {
			if(toVisit.isDefined) {
				val toVisitAfter = toVisit.map(_.flatMap( cz => getClosestDots(cz.id) ).flatten)
				simMat --= toVisit.get.map(_.id)
				if(toVisitAfter.isDefined) {
					if(!toVisitAfter.get.isEmpty) {
						clusters(clusterID) ++= toVisitAfter.get
						feedClusters(toVisitAfter)
					}
					else {
						clusterID += 1
						feedClusters(
							Try{
								val (newCz, _) = simMat.head._2.head
								val newBegin = mutable.HashSet(newCz)
								clusters += ((clusterID, newBegin))
								newBegin
							}.toOption
						)
					}
				}
			}
			else Unit
		}

		feedClusters(toVisit)

		val sortedClustersByClusterizableID = mutable.ArrayBuffer(clusters.flatMap{ case (clusterID, cluster) => cluster.map(_.addClusterIDs(clusterID)) }.toBuffer.sortBy{ cz: Cz[O, V] => cz.id }.map( cz => (cz.id, (cz.v, cz.clusterIDs.last)) ):_*)
		(sortedClustersByClusterizableID, epsilon, clusterID)
	}


}
/**
 *
 */
final case class EpsilonProximity[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](final val epsilonChoice: String, final val metric: D[V]) extends EpsilonProximityAncestor[V, D[V], EpsilonProximityModel[V, D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximity

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): EpsilonProximityModel[V, D] = {
		val (clusters, epsilon, clusterNumber) = preModel(data)
		EpsilonProximityModel(clusters, epsilon, metric, clusterNumber, data.hashCode)
	}
}
/**
 *
 */
final case class EpsilonProximityScalar[D <: ContinuousDistance](final val epsilonChoice: String, final val metric: D) extends EpsilonProximityAncestor[ScalarVector, D, EpsilonProximityModelScalar[D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximityScalar

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, ScalarVector]]): EpsilonProximityModelScalar[D] = {
		val (clusters, epsilon, clusterNumber) = preModel(data)
		EpsilonProximityModelScalar(clusters, epsilon, metric, clusterNumber, data.hashCode)
	}
}
/**
 *
 */
final case class EpsilonProximityBinary[D <: BinaryDistance](final val epsilonChoice: String, final val metric: D) extends EpsilonProximityAncestor[BinaryVector, D, EpsilonProximityModelBinary[D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximityBinary

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, BinaryVector]]): EpsilonProximityModelBinary[D] = {
		val (clusters, epsilon, clusterNumber) = preModel(data)
		EpsilonProximityModelBinary(clusters, epsilon, metric, clusterNumber, data.hashCode)
	}
}
/**
 *
 */
final case class EpsilonProximityMixed[D <: MixedDistance](final val epsilonChoice: String, final val metric: D) extends EpsilonProximityAncestor[MixedVector, D, EpsilonProximityModelMixed[D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximityMixed

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, MixedVector]]): EpsilonProximityModelMixed[D] = {
		val (clusters, epsilon, clusterNumber) = preModel(data)
		EpsilonProximityModelMixed(clusters, epsilon, metric, clusterNumber, data.hashCode)
	}
}

