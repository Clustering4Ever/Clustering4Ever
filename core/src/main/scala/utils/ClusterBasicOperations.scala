package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.GenSeq
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.mixt.HammingAndEuclidean
import org.clustering4ever.util.VectorsAddOperationsImplicits._
import org.clustering4ever.vectors.{GVector, MixedVector, ScalarVector, BinaryVector}
/**
 *
 */
object ClusterBasicOperations extends Serializable {
	/**
	 * @return an existing or the theoritical point which minimize its distance from all others cluster members for any space
	 * When it is euclidean or hamming or both combined distance which is used, the linear way to compute the point is applied, aka the mean and/or majority vote
	 *
	 * A major contribution will be to find heuristics in non trivial case, ie not Hamming or Euclidean distance, for the moment the similarity matrix is compute (O(n<sup>2</sup>)), which will allow to drastically improve custom metrics and many many algorithms
	 */
	final def obtainCenter[V <: GVector[V], D <: Distance[V]](cluster: GenSeq[V], metric: D): V = {
	    metric match {
	      case euclidean: Euclidean => obtainMean(cluster.asInstanceOf[GenSeq[ScalarVector]]).asInstanceOf[V]
	      case hamming: Hamming => obtainMedian(cluster.asInstanceOf[GenSeq[BinaryVector]]).asInstanceOf[V]
	      case hammingAndEuclidean: HammingAndEuclidean => obtainMixtCenter(cluster.asInstanceOf[GenSeq[MixedVector]]).asInstanceOf[V]
	      // Look for concret point which minimize its distance to all others points
	      case _ => cluster.minBy( v1 => cluster.aggregate(0D)( (s, v2) => s + metric.d(v1, v2), (sum1, sum2) => sum1 + sum2 ) )
	    }
	}
	/**
	 *
	 */
	private[clustering4ever] final def transformPreMeanAndCastItRaw(preMean: Array[Double], clusterSize: Long) = preMean.map(_ / clusterSize)
	/**
	 *
	 */
	private[clustering4ever] final def transformPreMeanAndCastIt(preMean: ScalarVector, clusterSize: Long) = ScalarVector(transformPreMeanAndCastItRaw(preMean.vector, clusterSize))
	/**
	 * It has sense only with Euclidean distance
	 * 
	 * @return the centroid of the given cluster composed by real vectors
	 */
	final def obtainMean(cluster: GenSeq[Array[Double]]): Array[Double] = transformPreMeanAndCastItRaw(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 * It has sense only with Euclidean distance
	 * 
	 * @return the centroid of the given cluster composed by real vectors
	 */
	final def obtainMean(cluster: GenSeq[ScalarVector]): ScalarVector = transformPreMeanAndCastIt(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 *
	 */
	private[clustering4ever] final def transformPreModeAndCastIt(preMode: Array[Int], clusterSize: Long) = preMode.map( v => if( 2 * v >= clusterSize ) 1 else 0 )
	/**
	 *
	 */
	private[clustering4ever] final def transformPreModeAndCastIt(preMode: BinaryVector, clusterSize: Long) = BinaryVector(preMode.vector.map( v => if( 2 * v >= clusterSize ) 1 else 0 ))
	/**
	 * It has sense only with Hamming distance
	 * 
	 * @return the centroid of the given cluster composed by binary vectors
	 */
	final def obtainMedian(cluster: GenSeq[Array[Int]]): Array[Int] = transformPreModeAndCastIt(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 * It has sense only with Hamming distance
	 * 
	 * @return the centroid of the given cluster composed by binary vectors
	 */
	final def obtainMedian(cluster: GenSeq[BinaryVector]): BinaryVector = transformPreModeAndCastIt(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 *
	 */
	final def obtainMixtCenter(cluster: GenSeq[MixedVector]): MixedVector = {
	 	val mixtVector: MixedVector = SumVectors.sumColumnMatrix(cluster)
	 	val binaryPart = transformPreModeAndCastIt(mixtVector.binary, cluster.size)
	 	val realPart = transformPreMeanAndCastItRaw(mixtVector.scalar, cluster.size)
	 	MixedVector(binaryPart, realPart)
	}

}