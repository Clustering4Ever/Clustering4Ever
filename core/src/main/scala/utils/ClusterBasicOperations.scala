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
	      case euclidean: Euclidean[_] => obtainMean(cluster.asInstanceOf[GenSeq[ScalarVector[Seq[Double]]]]).asInstanceOf[V]
	      case hamming: Hamming[_] => obtainMedian(cluster.asInstanceOf[GenSeq[BinaryVector[Seq[Int]]]]).asInstanceOf[V]
	      case hammingAndEuclidean: HammingAndEuclidean[_, _] => obtainMixtCenter(cluster.asInstanceOf[GenSeq[MixedVector[Seq[Int], Seq[Double]]]]).asInstanceOf[V]
	      // Look for concret point which minimize its distance to all others points
	      case _ => cluster.minBy( v1 => cluster.aggregate(0D)( (s, v2) => s + metric.d(v1, v2), (sum1, sum2) => sum1 + sum2 ) )
	    }
	}
	/**
	 *
	 */
	private final def transformPreMeanAndCastItRaw[V <: Seq[Double]](preMean: V, clusterSize: Int) = preMean.map(_ / clusterSize).asInstanceOf[V]
	/**
	 *
	 */
	private final def transformPreMeanAndCastIt[V <: Seq[Double]](preMean: ScalarVector[V], clusterSize: Int) = ScalarVector(transformPreMeanAndCastItRaw(preMean.vector, clusterSize))
	/**
	 * It has sense only with Euclidean distance
	 * 
	 * @return the centroid of the given cluster composed by real vectors
	 */
	final def obtainMean[V <: Seq[Double]](cluster: GenSeq[V]): V = transformPreMeanAndCastItRaw(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 * It has sense only with Euclidean distance
	 * 
	 * @return the centroid of the given cluster composed by real vectors
	 */
	final def obtainMean[V <: Seq[Double]](cluster: GenSeq[ScalarVector[V]]): ScalarVector[V] = transformPreMeanAndCastIt(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 *
	 */
	private final def transformPreModeAndCastIt[V <: Seq[Int]](preMode: V, clusterSize: Int) = preMode.map( v => if( 2 * v >= clusterSize ) 1 else 0 ).asInstanceOf[V]
	/**
	 *
	 */
	private final def transformPreModeAndCastIt[V <: Seq[Int]](preMode: BinaryVector[V], clusterSize: Int) = BinaryVector(preMode.vector.map( v => if( 2 * v >= clusterSize ) 1 else 0 ).asInstanceOf[V])
	/**
	 * It has sense only with Hamming distance
	 * 
	 * @return the centroid of the given cluster composed by binary vectors
	 */
	final def obtainMedian[V <: Seq[Int]](cluster: GenSeq[V]): V = transformPreModeAndCastIt(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 * It has sense only with Hamming distance
	 * 
	 * @return the centroid of the given cluster composed by binary vectors
	 */
	final def obtainMedian[V <: Seq[Int]](cluster: GenSeq[BinaryVector[V]]): BinaryVector[V] = transformPreModeAndCastIt(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 *
	 */
	final def obtainMixtCenter[Vb <: Seq[Int], Vs <: Seq[Double]](cluster: GenSeq[MixedVector[Vb, Vs]]): MixedVector[Vb, Vs] = {
	 	val mixtVector: MixedVector[Vb, Vs] = SumVectors.sumColumnMatrix(cluster)
	 	val binaryPart = transformPreModeAndCastIt(mixtVector.binary, cluster.size)
	 	val realPart = transformPreMeanAndCastItRaw(mixtVector.scalar, cluster.size)
	 	MixedVector(binaryPart, realPart)
	}

}