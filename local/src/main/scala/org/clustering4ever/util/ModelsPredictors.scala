package org.clustering4ever.util

/**
 * @author Beck Gaël
 */
import org.clustering4ever.clustering.kfamily.kcenters.KMeansModel
import org.clustering4ever.distances.ContinuousDistance
import org.clustering4ever.roottraits.{Clusterizable, GVector, ScalarVector}
import shapeless.Poly1

import scala.collection.GenSeq
import scala.language.higherKinds
/**
 *
 */
trait OneVectorPredictor[V <: GVector[V]] {
	val v: V
}
/**
 *
 */
trait DatasetPredictor[O, V <: GVector[V], Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]] {
	val data: GS[Cz[O, V]]
}
/**
 *
 */
trait KmeansModelsPredictorOneVector extends OneVectorPredictor[ScalarVector] with Poly1 {
	implicit def kMeans[D <: ContinuousDistance] = at[KMeansModel[D]](_.centerPredict(v))
}
/**
 *
 */
trait KmeansModelsPredictorDataset[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]] extends DatasetPredictor[O, ScalarVector, Cz, GS] with Poly1 {
	implicit def kMeans[D <: ContinuousDistance] = at[KMeansModel[D]](_.centerPredict(data))
}