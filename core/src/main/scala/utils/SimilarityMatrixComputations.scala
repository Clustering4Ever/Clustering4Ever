package clustering4ever.util

import scala.collection.{mutable, immutable, GenSeq}
import clustering4ever.math.distances.Distance
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean

object SimilarityMatrix {
	def simpleSimilarityMatrix[Obj](data: Seq[Obj], metric: Distance[Obj]) : Seq[(Obj, Seq[Double])] = data.map( v1 => (v1, data.map( v2 => metric.d(v1, v2) )) )

	def similarityMatrix[ID: Numeric, Obj](data: Seq[(ID, Obj)], metric: Distance[Obj]) : scala.collection.Map[ID, Seq[(ID, Double)]] =
		data.map{ case (id1, v1) => id1 -> data.map{ case (id2, v2) => (id2, metric.d(v1, v2)) }}.toMap.seq

	def sortedSimilarityMatrix[ID: Numeric, Obj](data: Array[(ID, Obj)], metric: Distance[Obj]) : scala.collection.Map[ID, Seq[(ID, Double)]] =
		similarityMatrix(data, metric).map{ case (id, dist) => (id, dist.seq.sortBy(_._2)) }

	def similarityMatrixWithVector[ID: Numeric, Obj](data: GenSeq[(ID, Obj)], metric: Distance[Obj]) : scala.collection.GenMap[ID, GenSeq[(ID, Obj, Double)]] =
		data.map{ case (id1, v1) => id1 -> data.map{ case (id2, v2) => (id2, v2, metric.d(v1, v2)) }}.toMap

	def sortedSimilarityMatrixWithVector[ID: Numeric, Obj](data: GenSeq[(ID, Obj)], metric: Distance[Obj]) : scala.collection.GenMap[ID, Seq[(ID, Obj, Double)]] =
		similarityMatrixWithVector(data, metric).map{ case (id, dist) => (id, dist.seq.sortBy(_._3)) }
}