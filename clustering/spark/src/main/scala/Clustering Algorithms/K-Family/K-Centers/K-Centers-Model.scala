package org.clustering4ever.spark.clustering.kcenters
/**
 * Beck Gaël
 */
import scala.collection.mutable
import org.clustering4ever.clustering.CenterOrientedModelLocalClusterizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.scala.vectors.GVector
/**
 *
 */
class KCentersModel[V <: GVector, D <: Distance[V]](val centers: mutable.HashMap[Int, V], val metric: D) extends CenterOrientedModelLocalClusterizable[V, D]