package clustering4ever.scala.clustering.kmeans

import scala.collection.{mutable, immutable, GenSeq}
import clustering4ever.clustering.CommonPredictClusteringModel
import clustering4ever.math.distances.{ContinuousDistance, Distance}
import clustering4ever.scala.clusterizables.RealClusterizable
import scala.reflect.ClassTag
import clustering4ever.scala.clustering.KCommonsModel
/**
 * @author Beck Gaël
 */
sealed abstract class KMeansModel[
	ID: Numeric,
	V <: Seq[Double] : ClassTag,
	Obj
](
	centers: mutable.HashMap[Int, V],
	metric: ContinuousDistance[V]
) extends KCommonsModel[
	ID,
	Double,
	V,
	ContinuousDistance[V],
	RealClusterizable[ID, Obj, V]
](centers, metric)

final class KMeansModelSeq[ID: Numeric, Obj](centers: mutable.HashMap[Int, Seq[Double]], metric: ContinuousDistance[Seq[Double]]) extends KMeansModel[ID, Seq[Double], Obj](centers, metric)

final class KMeansModelCustom[ID: Numeric, V <: Seq[Double] : ClassTag, Obj](centers: mutable.HashMap[Int, V], metric: ContinuousDistance[V]) extends KMeansModel[ID, V, Obj](centers, metric)