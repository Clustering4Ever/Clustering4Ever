package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.GenSeq
import org.clustering4ever.math.distances.{GenericDistance, Distance, ContinuousDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.GVector
/**
 *
 */
object SimilarityMatrix extends Serializable {
	/**
	 *
	 */
	def simpleSimilarityMatrix[O, D <: GenericDistance[O]](data: GenSeq[O], metric: D): GenSeq[(O, GenSeq[Double])] = data.map( v1 => (v1, data.map( altVectors => metric.d(v1, altVectors) )) )
	/**
	 * @return object in the given dataset which minimize its distance to every other points 
	 */
	def distanceMinimizer[O, D <: GenericDistance[O]](data: GenSeq[O], metric: D): O = simpleSimilarityMatrix(data, metric).minBy{ case (_, distances) => distances.sum }._1
	/**
	 *
	 */
	def similarityMatrixWithVector[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V]](data: GenSeq[Cz[ID, O, V]], metric: D): scala.collection.GenMap[ID, GenSeq[(Cz[ID, O, V], Double)]] = {
		data.map( cz1 => cz1.id -> data.map( cz2 => (cz2, metric.d(cz1.v, cz2.v)) ) ).toMap
	}
	/**
	 *
	 */
	def sortedSimilarityMatrixWithVector[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V]](data: GenSeq[Cz[ID, O, V]], metric: D): scala.collection.GenMap[ID, Seq[(Cz[ID, O, V], Double)]] = {
		similarityMatrixWithVector(data, metric).map{ case (id, dist) => (id, dist.seq.sortBy(_._2)) }
	}
}