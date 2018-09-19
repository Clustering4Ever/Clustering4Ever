package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.collection.GenSeq
import clustering4ever.math.distances.Distance
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean

object SimilarityMatrix {

	def simpleSimilarityMatrix[O](data: GenSeq[O], metric: Distance[O]): GenSeq[(O, GenSeq[Double])] = data.map( v1 => (v1, data.map( v2 => metric.d(v1, v2) )) )

	def similarityMatrix[ID: Numeric, O](data: Seq[(ID, O)], metric: Distance[O]): scala.collection.Map[ID, Seq[(ID, Double)]] =
		data.map{ case (id1, v1) => id1 -> data.map{ case (id2, v2) => (id2, metric.d(v1, v2)) } }.toMap.seq

	def sortedSimilarityMatrix[ID: Numeric, O](data: Array[(ID, O)], metric: Distance[O]): scala.collection.Map[ID, Seq[(ID, Double)]] =
		similarityMatrix(data, metric).map{ case (id, dist) => (id, dist.seq.sortBy(_._2)) }

	def similarityMatrixWithVector[ID: Numeric, O](data: GenSeq[(ID, O)], metric: Distance[O]) : scala.collection.GenMap[ID, GenSeq[(ID, O, Double)]] =
		data.map{ case (id1, v1) => id1 -> data.map{ case (id2, v2) => (id2, v2, metric.d(v1, v2)) } }.toMap

	def sortedSimilarityMatrixWithVector[ID: Numeric, O](data: GenSeq[(ID, O)], metric: Distance[O]): scala.collection.GenMap[ID, Seq[(ID, O, Double)]] =
		similarityMatrixWithVector(data, metric).map{ case (id, dist) => (id, dist.seq.sortBy(_._3)) }
}