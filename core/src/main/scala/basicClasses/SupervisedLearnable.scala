package org.clustering4ever.preprocessing
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import org.clustering4ever.scala.vectorizables.Vectorizable
import org.clustering4ever.scala.clusterizables.IdentifiedVectorGen
/**
 * DFCL works for Discrete Features Classification Learnable
 */
class DFCLG[ID, O, V <: Seq[_]](val id: ID, val vectorizable: Vectorizable[O], val label: Int, val workingVector: V) extends IdentifiedVectorGen[ID, O, V] {
	/**
	 * Transform from DFCL to HDFCL by applying bucketing of bucketsOfFeats argument 
	 */
	def bucketizedFeatures[T[X] <: Seq[X], U[Y] <: Seq[Y]](bucketsOfFeats: U[T[Int]]): HDFCL[ID, Int, T, U] = {
		val newWorkingVector = bucketsOfFeats.map(_.map(workingVector(_))).asInstanceOf[U[T[Int]]]
		new HDFCL(id, label, newWorkingVector)
	}
} 
/**
 * DFCL works for Discrete Features Classification Learnable
 */
class DFCL[ID, V <: Seq[_]](id: ID, label: Int, workingVector: V) extends DFCLG(id, new Vectorizable(workingVector), label, workingVector)
/**
 * Special class for Rough Set heuristic
 */
class HDFCL[ID, U, T[X] <: Seq[X], V[Y] <: Seq[Y]](id: ID, label: Int, workingVector: V[T[U]]) extends DFCL(id, label, workingVector) {
	/**
	 * @return DFCL with a specific column of features changing features value by 0..n-1
	 */
	def getOneFeaturesBucket(idx: Int) = {
		val newWorkingVector = workingVector(idx).zipWithIndex.map(_._2).asInstanceOf[T[U]]
		new DFCL(id, label, newWorkingVector)
	}
	/**
	 * @return DFCL with features flatten
	 */
	def flatten = {
		val newWorkingVector = workingVector.flatten
		new DFCL(id, label, newWorkingVector)
	}
}
