package clustering4ever.util

import scala.collection.{mutable, immutable, GenSeq}
import clustering4ever.math.distances.Distance
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean

object SimilarityMatrix
{
	def simpleSimilarityMatrix[Obj](data: GenSeq[Obj], metric: Distance[Obj]) : GenSeq[(Obj, GenSeq[Double])] =
		data.map( v1 => (v1, data.map( v2 => metric.d(v1, v2) )) )

	def similarityMatrix[ID: Numeric, Obj](data: GenSeq[(ID, Obj)], metric: Distance[Obj]) : scala.collection.Map[ID, GenSeq[(ID, Double)]] =
		data.map{ case (id1, v1) => id1 -> data.map{ case (id2, v2) => (id2, metric.d(v1, v2)) }}.toMap.seq

	def similarityMatrixElkan[ID: Numeric](data: mutable.HashMap[ID, immutable.Vector[Double]]) : mutable.HashMap[ID, Array[(ID, immutable.Vector[Double], Double)]] =
	{
		val metric = new Euclidean[Seq[Double]](true)
		for( (id1, v1) <- data) yield id1 -> (for( (id2, v2) <- data.toArray if( id1 != id2 ) ) yield (id2, v2, metric.d(v1, v2) / 2D)).sortBy(_._3)
		//data.map{ case (id1, v1) => id1 -> (data.map{ case (id2, v2) => if( id1 != id2 ) ) yield (id2, v2, metric.d(v1, v2) / 2D)).sortBy(_._3)

	}

	def sortedSimilarityMatrix[ID: Numeric, Obj](data: Array[(ID, Obj)], metric: Distance[Obj]) : scala.collection.Map[ID, Seq[(ID, Double)]] =
		similarityMatrix(data, metric).map{ case (id, dist) => (id, dist.seq.sortBy(_._2)) }

	def similarityMatrixWithVector[ID: Numeric, Obj](data: GenSeq[(ID, Obj)], metric: Distance[Obj]) : scala.collection.Map[ID, GenSeq[(ID, Obj, Double)]] =
		data.map{ case (id1, v1) => id1 -> data.map{ case (id2, v2) => (id2, v2, metric.d(v1, v2)) }}.toMap.seq

	def sortedSimilarityMatrixWithVector[ID: Numeric, Obj](data: GenSeq[(ID, Obj)], metric: Distance[Obj]) : scala.collection.Map[ID, Seq[(ID, Obj, Double)]] =
		similarityMatrixWithVector(data, metric).map{ case (id, dist) => (id, dist.seq.sortBy(_._3)) }
}