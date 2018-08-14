package clustering4ever.util

import scala.collection.{mutable, immutable}
import clustering4ever.math.distances.Distance
import clustering4ever.math.distances.ContinuousDistances
import clustering4ever.math.distances.scalar.Euclidean

object SimilarityMatrix
{
	def simpleSimilarityMatrix[Obj](data: Seq[Obj], metric: Distance[Obj]) : Seq[(Obj, Seq[Double])] =
	{
		for( v1 <- data ) yield (v1, for( v2 <- data ) yield (metric.d(v1, v2)))
	}

	def similarityMatrix[ID: Numeric, Obj](data: Array[(ID, Obj)], metric: Distance[Obj]) : mutable.HashMap[ID, Array[(ID, Double)]] =
	{
		for( (id1, v1) <- mutable.HashMap(data:_*)) yield ( id1 -> (for( (id2, v2) <- data ) yield ((id2, metric.d(v1, v2))) ) )
	}

	def similarityMatrixElkan[ID: Numeric](data: mutable.HashMap[ID, immutable.Vector[Double]]) : mutable.HashMap[ID, Array[(ID, immutable.Vector[Double], Double)]] =
	{
		val metric = new Euclidean(true)
		for( (id1, v1) <- data) yield id1 -> (for( (id2, v2) <- data.toArray if( id1 != id2 ) ) yield (id2, v2, metric.d(v1, v2) / 2D)).sortBy(_._3)
	}

	def sortedSimilarityMatrix[ID: Numeric, Obj](data: Array[(ID, Obj)], metric: Distance[Obj]) : mutable.HashMap[ID, Array[(ID, Double)]] =
	{
		val rawSM = similarityMatrix(data, metric)
		rawSM.map{ case (id, dist) => (id, dist.sortBy(_._2)) }
	}

	def similarityMatrixWithVector[ID: Numeric, Obj](data: Array[(ID, Obj)], metric: Distance[Obj]) : mutable.HashMap[ID, Array[(ID, Obj, Double)]] =
	{
		for( (id1, v1) <- mutable.HashMap(data:_*) ) yield ( id1 -> (for( (id2, v2) <- data ) yield ((id2, v2, metric.d(v1, v2))) ) )
	}

	def sortedSimilarityMatrixWithVector[ID: Numeric, Obj](data: Array[(ID, Obj)], metric: Distance[Obj]) : mutable.HashMap[ID, Array[(ID, Obj, Double)]] =
	{
		val rawSM = similarityMatrixWithVector(data, metric)
		rawSM.map{ case (id, dist) => (id, dist.sortBy(_._3)) }
	}
}