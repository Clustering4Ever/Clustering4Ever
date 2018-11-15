package org.clustering4ever.preprocessing
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import org.clustering4ever.scala.vectorizables.{Vectorizable, Vector}
import org.clustering4ever.scala.clusterizables.{IdentifiedVectorizable, IdentifiedVector}
/**
 * DFCL works for Discrete Features Classification Learnable
 */
class DFCLG[ID: Numeric, O, V <: Seq[_]](id: ID, vectorizable: Vectorizable[O, V], val label: Int) extends IdentifiedVectorizable[ID, O, V](id, vectorizable) {
	/**
	 * Transform from DFCL to HDFCL by applying bucketing of bucketsOfFeats argument 
	 */
	def bucketizedFeatures[T[Int] <: Seq[Int], U[T] <: Seq[T]](bucketsOfFeats: U[T[Int]]): HDFCL[ID, Int, T, U] = {
		val bucketizedVector = new Vector(bucketsOfFeats.map(_.map(originalVector(_))).asInstanceOf[U[T[Int]]])
		new HDFCL(id, bucketizedVector, label)
	}
} 
/**
 * DFCL works for Discrete Features Classification Learnable
 */
class DFCL[ID: Numeric, V <: Seq[_]](id: ID, vectorizable: Vector[V], label: Int) extends DFCLG(id, vectorizable, label)
/**
 * Special class for Rough Set heuristic
 */
class HDFCL[ID: Numeric, U, T[U] <: Seq[U], V[T] <: Seq[T]](id: ID, vectorizable: Vector[V[T[U]]], label: Int) extends DFCL(id, vectorizable, label) {
	/**
	 * @return DFCL with a specific column of features changing features value by 0..n-1
	 */
	def getOneFeaturesBucket(idx: Int) = new DFCL(id, new Vector(originalVector(idx).zipWithIndex.map(_._2).asInstanceOf[T[U]]), label)
	/**
	 * @return DFCL with features flatten
	 */
	def flatten = new DFCL(id, new Vector(originalVector.flatten), label)

}
