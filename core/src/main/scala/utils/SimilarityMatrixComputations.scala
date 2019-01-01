package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.collection.GenSeq
import spire.math.{Numeric => SNumeric}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import scala.language.higherKinds
/**
 *
 */
object SimilarityMatrix {
	/**
	 *
	 */
	def simpleSimilarityMatrix[O, D <: Distance[O]](data: GenSeq[O], metric: D): GenSeq[(O, GenSeq[Double])] = data.map( v1 => (v1, data.map( altVectors => metric.d(v1, altVectors) )) )
	/**
	 *
	 */
	def distanceMinimizer[O, D <: Distance[O]](data: GenSeq[O], metric: D): O = simpleSimilarityMatrix(data, metric).minBy{ case (_, distances) => distances.sum }._1
	/**
	 *
	 */
	def similarityMatrixWithVector[ID, O, D <: Distance[O]](data: GenSeq[(ID, O)], metric: D): scala.collection.GenMap[ID, GenSeq[(ID, O, Double)]] = {
		data.map{ case (id1, v1) => id1 -> data.map{ case (id2, altVectors) => (id2, altVectors, metric.d(v1, altVectors)) } }.toMap
	}
	/**
	 *
	 */
	def sortedSimilarityMatrixWithVector[ID, O, D <: Distance[O]](data: GenSeq[(ID, O)], metric: D): scala.collection.GenMap[ID, Seq[(ID, O, Double)]] = {
		similarityMatrixWithVector(data, metric).map{ case (id, dist) => (id, dist.seq.sortBy(_._3)) }
	}
}