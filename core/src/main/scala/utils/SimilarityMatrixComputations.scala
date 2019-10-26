package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{GenSeq, mutable}
import scala.collection.parallel.mutable.ParArray
import org.clustering4ever.math.distances.{GenericDistance, Distance, ContinuousDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.preprocessing.Preprocessable
import org.clustering4ever.vectors.GVector
/**
 *
 */
object SimilarityMatrix extends Serializable {
	/**
	 *
	 */
	final def simpleSimilarityMatrix[O, D <: GenericDistance[O]](data: GenSeq[O], metric: D): GenSeq[(O, GenSeq[Double])] = data.map( v1 => (v1, data.map( altVectors => metric.d(v1, altVectors) )) )
	/**
	 * @return object in the given dataset which minimize its distance to every other points 
	 */
	final def distanceMinimizer[O, D <: GenericDistance[O]](data: GenSeq[O], metric: D): O = simpleSimilarityMatrix(data, metric).minBy{ case (_, distances) => distances.sum }._1
	/**
	 *
	 */
	final def similarityMatrixWithVector[O, V <: GVector[V], Cz[Y, Z <: GVector[Z]] <: Preprocessable[Y, Z, Cz], D <: Distance[V]](data: GenSeq[Cz[O, V]], metric: D): scala.collection.GenMap[Long, GenSeq[(Cz[O, V], Double)]] = {
		data.map( cz1 => cz1.id -> data.map( cz2 => (cz2, metric.d(cz1.v, cz2.v)) ) ).toMap
	}
	/**
	 *
	 */
	final def sortedSimilarityMatrixWithVector[O, V <: GVector[V], Cz[Y, Z <: GVector[Z]] <: Preprocessable[Y, Z, Cz], D <: Distance[V]](data: GenSeq[Cz[O, V]], metric: D): scala.collection.GenMap[Long, Seq[(Cz[O, V], Double)]] = {
		similarityMatrixWithVector(data, metric).map{ case (id, dist) => (id, dist.seq.sortBy(_._2)) }
	}
	/**
	 *
	 */
	final def halfSimiliratyMatrix[T : ClassTag](data: Array[T], metric: (T, T) => Double): HalfSimilarityMatrix = {
		HalfSimilarityMatrix(data.zipWithIndex.foldLeft(mutable.ArrayBuffer.empty[Array[Double]]){ case (agg, (v, i)) => agg += data.take(i).map(metric(v, _)) }.toArray)
	}
	/**
	 *
	 */
	final def halfSimiliratyMatrixPA[T : ClassTag](data: ParArray[T], metric: (T, T) => Double): HalfSimilarityMatrix = {
		HalfSimilarityMatrix(data.zipWithIndex.foldLeft(mutable.ArrayBuffer.empty[Array[Double]]){ case (agg, (v, i)) => agg += data.take(i).toArray.map(metric(v, _)) }.toArray)
	}
}
/**
 *
 */
final case class HalfSimilarityMatrix(val matrix: Array[Array[Double]]) {
	/**
	 *
	 */
	def apply(x: Int, y: Int): Double = {
		require(x < matrix.size)
		require(y < matrix.head.size)
		if (x > y) matrix(x)(y)
		else matrix(y)(x)
	}
	/**
	 *
	 */
	def distanceFromIdx(idx: Int): Array[Double] = {
		val buff = mutable.ArrayBuffer.empty[Double]
		val incompleteRow = matrix(idx)
		val rowComplement = (idx + 1 until matrix.size).map( i => matrix(i)(idx) ).toArray
		buff ++= incompleteRow
		buff += 0D
		buff ++= rowComplement
		buff.toArray
	}
	/**
	 *
	 */
	def indexedDistanceFromIdx(idx: Int): Array[(Double, Int)] = distanceFromIdx(idx).zipWithIndex
}
