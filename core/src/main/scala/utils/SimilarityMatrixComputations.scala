package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.collection.GenSeq
import spire.math.{Numeric => SNumeric}
import clustering4ever.math.distances.{Distance, DistanceSeq, ContinuousDistance}
import clustering4ever.math.distances.scalar.Euclidean
import scala.language.higherKinds
/**
 *
 */
object SimilarityMatrix {
	/**
	 *
	 */
	def simpleSimilarityMatrix[O, D <: Distance[O]](data: GenSeq[O], metric: D): GenSeq[(O, GenSeq[Double])] = data.map( v1 => (v1, data.map( v2 => metric.d(v1, v2) )) )
	/**
	 *
	 */
	def simpleSimilarityMatrix[@specialized(Int, Double) N: SNumeric, V <: Seq[N], D <: DistanceSeq[N, V]](data: GenSeq[V], metric: D): GenSeq[(V, GenSeq[Double])] = simpleSimilarityMatrix(data, metric)
	/**
	 *
	 */
	def distanceMinimizer[O, D <: Distance[O]](data: GenSeq[O], metric: D): O = simpleSimilarityMatrix(data, metric).minBy{ case (_, distances) => distances.sum }._1
	/**
	 *
	 */
	def distanceMinimizer[@specialized(Int, Double) N: SNumeric, V <: Seq[N], D <: DistanceSeq[N, V]](data: GenSeq[V], metric: D): V = distanceMinimizer(data, metric)
	/**
	 *
	 */
	def similarityMatrixWithVector[ID: Numeric, O, D <: Distance[O]](data: GenSeq[(ID, O)], metric: D): scala.collection.GenMap[ID, GenSeq[(ID, O, Double)]] = {
		data.map{ case (id1, v1) => id1 -> data.map{ case (id2, v2) => (id2, v2, metric.d(v1, v2)) } }.toMap
	}
	/**
	 *
	 */
	def similarityMatrixWithVector[ID: Numeric, @specialized(Int, Double) N: SNumeric, V <: Seq[N], D <: DistanceSeq[N, V]](data: GenSeq[(ID, V)], metric: Distance[V]): scala.collection.GenMap[ID, GenSeq[(ID, V, Double)]] = {
		similarityMatrixWithVector(data, metric)
	}
	/**
	 *
	 */
	def sortedSimilarityMatrixWithVector[ID: Numeric, O, D[O] <: Distance[O]](data: GenSeq[(ID, O)], metric: D[O]): scala.collection.GenMap[ID, Seq[(ID, O, Double)]] = {
		similarityMatrixWithVector(data, metric).map{ case (id, dist) => (id, dist.seq.sortBy(_._3)) }
	}
	/**
	 *
	 */
	def sortedSimilarityMatrixWithVector[ID: Numeric, @specialized(Int, Double) N: SNumeric, V <: Seq[N], D <: DistanceSeq[N, V]](data: GenSeq[(ID, V)], metric: D): scala.collection.GenMap[ID, Seq[(ID, V, Double)]] = {
		sortedSimilarityMatrixWithVector(data, metric)
	}
}