package org.clustering4ever.clustering.optics

import org.clustering4ever.clusteringtraits.ClusteringAlgorithmLocal
import org.clustering4ever.distances.{BinaryDistance, ContinuousDistance, Distance, MixedDistance}
import org.clustering4ever.roottraits._
import scalaz.Scalaz._
import scalaz._

import scala.collection.GenSeq
import scala.language.higherKinds

/**
  * @tparam V
  * @tparam D
  * @tparam CM
  */
trait OpticsAncestor[V <: GVector[V], D <: Distance[V], CM <: OpticsModelAncestor[V, D]] extends ClusteringAlgorithmLocal[V, CM] {

  def metric: D

  /** Compute the OPTICS Model
    *
    * @param eps The maximum distance between two samples for one to be considered as in the neighborhood of the other. Reducing eps will result in shorter run times.
    * @param minPts The number of samples in a neighborhood for a point to be considered as a core point. Also, up and down steep regions can’t have more then min_samples consecutive non-steep points. Expressed as an absolute number or a fraction of the number of samples (rounded to be at least 2).
    * @return The cluster ordered list of sample indices.
    */
  private[optics] final def obtainOrder[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]], eps: Double, minPts: Int): List[(Long, Option[Double])] = {
    final case class OpticsPoint(clusterizable: Cz[O, V], var reacheabilityDistance: Option[Double] = None, var processed: Boolean = false)

    val db = data.map(OpticsPoint(_)).toList

    /** Give the neighborhood of an point with eps as limit */
    def neighbors(p: OpticsPoint) : List[OpticsPoint] =
      db.filter(p2 => metric.d(p.clusterizable.v, p2.clusterizable.v) <= eps)


    /** MinPtsth closest point */
    def coreDistance(p: OpticsPoint): Option[Double] = {
      val n = neighbors(p).map(d => metric.d(p.clusterizable.v, d.clusterizable.v))
      if (n.length < minPts) None else  Some(n.min)
    }

    /**  The priority queue Seeds is updated with the eps eps-neighborhood of p and q */
    @annotation.tailrec
    def update(neighbors: List[OpticsPoint], p: OpticsPoint, seeds: Heap[OpticsPoint]): Heap[OpticsPoint] = {
  
      implicit val orderedDatum: Order[OpticsPoint] = Order.orderBy(_.reacheabilityDistance.getOrElse(Double.PositiveInfinity))
  
      neighbors match {
        case head :: tail if !head.processed =>
          val newReachabilityDistance = Seq(coreDistance(p), Some(metric.d(p.clusterizable.v, head.clusterizable.v))).flatten.max
          head match {
            case OpticsPoint(_, None, _) =>
              head.reacheabilityDistance = Some(newReachabilityDistance)
              update(tail, p, seeds + head)
            case OpticsPoint(_, Some(reacheabilityDistance), _) if newReachabilityDistance < reacheabilityDistance =>
              head.reacheabilityDistance = Some(newReachabilityDistance)
              update(tail, p, seeds + head)
          }
        case head :: tail if head.processed => update(tail, p, seeds)
        case Nil => seeds
      }
    }

    /** Expand the cluster order */
    @annotation.tailrec
    def expandClusterOrder(seeds: Heap[OpticsPoint], order: List[OpticsPoint]): List[OpticsPoint] = {
      seeds.uncons match {
        case Some((q, remainder)) =>
          q.processed = true
          if (coreDistance(q).isEmpty)
            expandClusterOrder(update(neighbors(q), q, remainder), q :: order)
          else
            expandClusterOrder(remainder, q :: order)
        case None => order
      }
    }

    /** Run OPTICS algorithm */
    @annotation.tailrec
    def mainLoop(db: List[OpticsPoint], order: List[OpticsPoint] = Nil): List[OpticsPoint] = {
      db match {
        case p :: tail if !p.processed =>
          p.processed = true
          if (coreDistance(p).isDefined) {
            mainLoop(tail, expandClusterOrder(update(neighbors(p), p, Heap.Empty[OpticsPoint]), p :: order))
          } else {
            mainLoop(tail, p :: order)
          }

        case head :: tail if head.processed => mainLoop(tail, order)
        case Nil => order.reverse
      }
    }

    mainLoop(db).map{
      case OpticsPoint(cz, rd, _) => cz.id -> rd
    }
  }
}


final case class Optics[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](metric: D[V], eps: Double, minPts: Int) extends OpticsAncestor[V, D[V], OpticsModel[V, D]] {
  
  val algorithmID = org.clustering4ever.roottraits.Optics

  def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): OpticsModel[V, D] = {
    OpticsModel(metric, obtainOrder(data, eps, minPts))
  }

}

final case class OpticsScalar[D <: ContinuousDistance](metric: D, eps: Double, minPts: Int) extends OpticsAncestor[ScalarVector, D, OpticsModelScalar[D]] {
  
  val algorithmID = org.clustering4ever.roottraits.OpticsScalar

  def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, ScalarVector]]): OpticsModelScalar[D] = {
    OpticsModelScalar(metric, obtainOrder(data, eps, minPts))
  }

}

final case class OpticsBinary[D <: BinaryDistance](metric: D, eps: Double, minPts: Int) extends OpticsAncestor[BinaryVector, D, OpticsModelBinary[D]] {
  
  val algorithmID = org.clustering4ever.roottraits.OpticsBinary

  def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, BinaryVector]]): OpticsModelBinary[D] = {
    OpticsModelBinary(metric, obtainOrder(data, eps, minPts))
  }

}

final case class OpticsMixed[D <: MixedDistance](metric: D, eps: Double, minPts: Int) extends OpticsAncestor[MixedVector, D, OpticsModelMixed[D]] {
  
  val algorithmID = org.clustering4ever.roottraits.OpticsMixed

  def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, MixedVector]]): OpticsModelMixed[D] = {
    OpticsModelMixed(metric, obtainOrder(data, eps, minPts))
  }

}





