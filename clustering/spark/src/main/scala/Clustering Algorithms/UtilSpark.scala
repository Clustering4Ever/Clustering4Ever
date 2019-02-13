package org.clustering4ever.sparktools

import scala.language.higherKinds
import org.apache.spark.rdd.RDD
import org.apache.spark.HashPartitioner
import scala.reflect.runtime.universe.TypeTag
import scala.util.Random
import scala.reflect.ClassTag
import scala.collection.{GenSeq, mutable}
import org.clustering4ever.preprocessing.Preprocessable
import org.clustering4ever.hashing.HashingScalar
import org.clustering4ever.vectors.{GVector, ScalarVector}

object UtilSpark
{

	type IndexPartition = Int
	type HasConverged = Boolean
    type IsOriginalDot = Boolean


	final def generateDataLocalityOnHashsedDS[
		O,
		V <: Seq[Double],
		Pz[B, C <: GVector[C]] <: Preprocessable[B, C, Pz]
	](
		rddToPartitioned: RDD[Pz[O, ScalarVector[V]]],
		nbblocs1: Int,
		nbBucketRange: Int
	): RDD[(IndexPartition, (Pz[O, ScalarVector[V]], IsOriginalDot, HasConverged))] = {
		val isOriginalPoint = true
		val hasConverged = true
		val bucketRange = 1 to nbBucketRange

		val lshRDD = rddToPartitioned.map((_, isOriginalPoint, !hasConverged))

		val localityPerPartitionRDD = lshRDD.mapPartitionsWithIndex{ (idx, it) =>
			val ar = it.toList
			def rightNeighbourhood = ar.flatMap{ case (cz, _, _) => bucketRange.collect{ case i if(idx + i < nbblocs1) => (idx + i, (cz, !isOriginalPoint, !hasConverged)) } }
			def leftNeighbourhood = ar.flatMap{ case (cz, _, _) => bucketRange.collect{ case i if(idx - i >= 0) => (idx - i, (cz, !isOriginalPoint, !hasConverged)) } }
			val composing = if(idx == 0) ar.map((idx, _)) ::: rightNeighbourhood
				else if(idx == nbblocs1 - 1) ar.map((idx, _)) ::: leftNeighbourhood
				else ar.map((idx, _)) ::: leftNeighbourhood ::: rightNeighbourhood

	      composing.toIterator

	    }.partitionBy(new HashPartitioner(nbblocs1))
	    
	    localityPerPartitionRDD
	}

	final def generateDataLocalityLD[
		O,
		V <: Seq[Double],
		Pz[B, C <: GVector[C]] <: Preprocessable[B, C, Pz],
		Hasher <: HashingScalar[V]
	](
		rddToPartitioned: RDD[Pz[O, ScalarVector[V]]],
		hashing: Hasher,
		nbblocs1: Int,
		nbBucketRange: Int
	): RDD[(IndexPartition, (Pz[O, ScalarVector[V]], IsOriginalDot, HasConverged))] = {
		val hashedRDD = rddToPartitioned.sortBy( cz => hashing.hf(cz.v) , ascending = true, nbblocs1 )
		generateDataLocalityOnHashsedDS(hashedRDD, nbblocs1, nbBucketRange)
	}

}