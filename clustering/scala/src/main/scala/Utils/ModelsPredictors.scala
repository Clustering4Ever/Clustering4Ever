package org.clustering4ever.clustering.predictors
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.GenSeq
import shapeless.{HList, HNil}
import org.clustering4ever.shapeless.HListRelated
import org.clustering4ever.clustering.ClusteringModel
import shapeless.{Poly, Poly1}
import shapeless.ops.hlist.Mapper
import org.clustering4ever.clustering.{ClusteringModelLocal, ClusteringModelLocalScalar, ClusteringModelLocalBinary}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.clustering.kcenters.scala.{KCentersModel, KMeansModel, KModesModel, KPrototypesModels}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.clusterizables.Clusterizable
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
trait KmeansModelsPredictorOneVector[V <: Seq[Double]] extends OneVectorPredictor[ScalarVector[V]] with Poly1 {
	implicit def kMeans[D[X <: Seq[Double]] <: ContinuousDistance[X]] = at[KMeansModel[V, D]](_.centerPredict(v))
}
/**
 *
 */
trait KmeansModelsPredictorDataset[O, V <: Seq[Double], Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]] extends DatasetPredictor[O, ScalarVector[V], Cz, GS] with Poly1 {
	implicit def kMeans[D[X <: Seq[Double]] <: ContinuousDistance[X]] = at[KMeansModel[V, D]](_.centerPredict(data))
}
/**
 *
 */
// object KMeansExample {

	// import org.clustering4ever.clustering.keeper.ModelsKeeper
	// import scala.collection.{immutable, mutable}
	// import org.clustering4ever.math.distances.scalar.Euclidean
	// import org.clustering4ever.shapeless.HListRelated


	// val collectionSize = 100
	// val exploredPoints = (0 until collectionSize).map( i => new KmeansModelsPredictorOneVector[mutable.ArrayBuffer[Double]] { val v = ScalarVector(mutable.ArrayBuffer[Double](i)) } )

	// val fakeCenters = immutable.HashMap(0 -> mutable.ArrayBuffer[Double](5), 1 -> mutable.ArrayBuffer[Double](9), 2 -> mutable.ArrayBuffer[Double](40)).map{ case (k, v ) => (k, ScalarVector(v)) }
	// val oneModel = KMeansModel[mutable.ArrayBuffer[Double], Euclidean](8, Euclidean[mutable.ArrayBuffer[Double]](false), 0.0001, 100, fakeCenters)

	// val modelsKeeper = ModelsKeeper(oneModel :: HNil)

	// val models: HList = modelsKeeper.models

	// object Objjj extends KmeansModelsPredictorOneVector[mutable.ArrayBuffer[Double]] { val v = ScalarVector(mutable.ArrayBuffer[Double](0)) }

	// exploredPoints.map( p => modelsKeeper.mapOverModels(p) )
	// modelsKeeper.mapOverModels(exploredPoints.head)
	// HListRelated.mapOverHList(models, exploredPoints.head)
	// HListRelated.mapOverHList(models, Objjj)

// }