package clustering4ever.util

import scala.collection.{mutable, immutable}
import clustering4ever.math.distances.Distance
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean

object SimilarityMatrix
{
	def simpleSimilarityMatrix[Obj](data: Seq[Obj], metric: Distance[Obj]) : Seq[(Obj, Seq[Double])] = data.map( v1 => (v1, data.map( v2 => metric.d(v1, v2) )) )

	def similarityMatrix[ID: Numeric, Obj](data: Seq[(ID, Obj)], metric: Distance[Obj]) : scala.collection.Map[ID, Seq[(ID, Double)]] =
		data.map{ case (id1, v1) => id1 -> data.map{ case (id2, v2) => (id2, metric.d(v1, v2)) }}.toMap.seq

	def similarityMatrixElkan[ID: Numeric](data: mutable.HashMap[ID, immutable.Vector[Double]]) : mutable.HashMap[ID, Array[(ID, immutable.Vector[Double], Double)]] =
	{
		val metric = new Euclidean[immutable.Seq[Double]](true)
		data.map{ case (id1, v1) => id1 -> data.collect{ case (id2, v2) if( id1 != id2 ) => (id2, v2, metric.d(v1, v2) / 2D) }.toArray.sortBy(_._3) }
	}

	def sortedSimilarityMatrix[ID: Numeric, Obj](data: Array[(ID, Obj)], metric: Distance[Obj]) : scala.collection.Map[ID, Seq[(ID, Double)]] =
		similarityMatrix(data, metric).map{ case (id, dist) => (id, dist.seq.sortBy(_._2)) }

	def similarityMatrixWithVector[ID: Numeric, Obj](data: Seq[(ID, Obj)], metric: Distance[Obj]) : scala.collection.Map[ID, Seq[(ID, Obj, Double)]] =
		data.map{ case (id1, v1) => id1 -> data.map{ case (id2, v2) => (id2, v2, metric.d(v1, v2)) }}.toMap.seq

	def sortedSimilarityMatrixWithVector[ID: Numeric, Obj](data: Seq[(ID, Obj)], metric: Distance[Obj]) : scala.collection.Map[ID, Seq[(ID, Obj, Double)]] =
		similarityMatrixWithVector(data, metric).map{ case (id, dist) => (id, dist.seq.sortBy(_._3)) }
}