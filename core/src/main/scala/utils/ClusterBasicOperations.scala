package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.GenSeq
import org.clustering4ever.math.distances.{GenericDistance, Distance}
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.mixt.HammingAndEuclidean
import org.clustering4ever.util.VectorsAddOperationsImplicits._
import org.clustering4ever.vectors.{GVector, MixtVector, ScalarVector, BinaryVector}
/**
 * Type Class is probably the solution to this casting meli melo, but it is hard to apply in such complex case without any usefull ressources
 */
object ClusterBasicOperations extends Serializable {
	/**
	 * @return the center which minimize its distance from all others cluster members for any space
	 * When it is euclidean or hamming distance which is used, the linear way to compute the center is applied, aka the mean and mode (majority vote)
	 * Tried to avoid cast with typeClass Medoid[O, D <: [O]] but didn't succeed to implement a generic form with Objects extending Medoid[...]
	 * Many attemps with defining type V <: Seq[Double] but implicit calls are refused
	 * Someone to render it more proper would be cool :)
	 */
	def obtainCenter[O, D <: GenericDistance[O]](cluster: GenSeq[O], metric: D): O = {
	    metric match {
	      case euclidean: Euclidean[_] => obtainMean(cluster.asInstanceOf[GenSeq[ScalarVector[Seq[Double]]]]).asInstanceOf[O]
	      case hamming: Hamming[_] => obtainMode(cluster.asInstanceOf[GenSeq[BinaryVector[Seq[Int]]]]).asInstanceOf[O]
	      case hammingAndEuclidean: HammingAndEuclidean[_, _] => obtainMixtCenter(cluster.asInstanceOf[GenSeq[MixtVector[Seq[Int], Seq[Double]]]]).asInstanceOf[O]
	      // Look for point which minimize its distance to all others points
	      case _ => cluster.minBy{ v1 => cluster.map(metric.d(v1, _)).sum }
	    }
	}
	/**
	 *
	 */
	private def transformPreMeanAndCastIt[V <: Seq[Double]](preMean: V, clusterSize: Int) = preMean.map(_ / clusterSize).asInstanceOf[V]
	/**
	 *
	 */
	private def transformPreMeanAndCastIt[V <: Seq[Double]](preMean: ScalarVector[V], clusterSize: Int) = ScalarVector(preMean.vector.map(_ / clusterSize).asInstanceOf[V])
	/**
	 * @return the centroid of the given cluster composed by real vectors
	 * 
	 * It has sense only with Euclidean distance
	 */
	def obtainMean[V <: Seq[Double]](cluster: GenSeq[V]): V = transformPreMeanAndCastIt(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 * @return the centroid of the given cluster composed by real vectors
	 * 
	 * It has sense only with Euclidean distance
	 */
	def obtainMean[V <: Seq[Double]](cluster: GenSeq[ScalarVector[V]]): ScalarVector[V] = transformPreMeanAndCastIt(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 *
	 */
	private def transformPreModeAndCastIt[V <: Seq[Int]](preMode: V, clusterSize: Int) = preMode.map( v => if( 2 * v >= clusterSize ) 1 else 0 ).asInstanceOf[V]
	/**
	 *
	 */
	private def transformPreModeAndCastIt[V <: Seq[Int]](preMode: BinaryVector[V], clusterSize: Int) = BinaryVector(preMode.vector.map( v => if( 2 * v >= clusterSize ) 1 else 0 ).asInstanceOf[V])
	/**
	 * @return the centroid of the given cluster composed by binary vectors
	 * 
	 * It has sense only with Hamming distance
	 */
	def obtainMode[V <: Seq[Int]](cluster: GenSeq[V]): V = transformPreModeAndCastIt(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 * @return the centroid of the given cluster composed by binary vectors
	 * 
	 * It has sense only with Hamming distance
	 */
	def obtainMode[V <: Seq[Int]](cluster: GenSeq[BinaryVector[V]]): BinaryVector[V] = transformPreModeAndCastIt(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 *
	 */
	 def obtainMixtCenter[Vb <: Seq[Int], Vs <: Seq[Double]](cluster: GenSeq[MixtVector[Vb, Vs]]): MixtVector[Vb, Vs] = {
	 	val mixtVector: MixtVector[Vb, Vs] = SumVectors.sumColumnMatrix(cluster)
	 	val binaryPart = transformPreModeAndCastIt(mixtVector.binary, cluster.size)
	 	val realPart = transformPreMeanAndCastIt(mixtVector.scalar, cluster.size)
	 	MixtVector(binaryPart, realPart)
	}

}