package org.clustering4ever.preprocessing.featureselection

/**
 * @author Beck Gaël
 */
import org.clustering4ever.roottraits._

import scala.collection.{GenSeq, immutable, mutable}
import scala.language.higherKinds
import scala.util.Random
/**
 * Theses functions are used to preprocess raw data                    
 */
object Util extends Serializable {
  /**
   * Determine in which interval falls a value given a specific range.
   * @return i if smaller or equal than ith value (starting at 0) and range.size if bigger than the last range value 
   */
  final def whichInterval[N](d: N, range: Seq[N])(implicit num: Ordering[N]): Int  = {
    @annotation.tailrec
    def go(i: Int): Int = {
      if(num.lteq(d, range(i))) i
      else go(i + 1)
    }
    if(num.gt(d, range.last)) range.size else go(0)
  }
  /**
   *
   */
  private final def obtainIdxByValueByFeatIdx[T](occurPerFeat: Array[mutable.HashSet[T]]) = { 
    immutable.HashMap(
      occurPerFeat.map( values => immutable.HashMap(values.toSeq.zipWithIndex:_*) )
      .zipWithIndex.map(_.swap)
    :_*)
  }
  /**
   *
   */
  private final def reductHashSeq[T](s1: Array[mutable.HashSet[T]], s2: Array[mutable.HashSet[T]]) = s1.zip(s2).map( x => x._1 ++ x._2 )
  /**
   *
   */
  private final def reduceOccFeaturesGs[T](gs: GenSeq[Array[mutable.HashSet[T]]]) = gs.reduce(reductHashSeq(_, _))
  /**
   *
   */
  final def obtainOccurencePerFeature[O, T, V[A] <: GSimpleVector[A, V[A]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](gs: GenSeq[Sz[O, V[T]]]): Array[mutable.HashSet[T]] = reduceOccFeaturesGs(gs.map(_.v.vector.map(mutable.HashSet(_))))
  /**
   *
   */
  final def obtainRandomlyBucketizedFeatures(numberOfFeatures: Int, numberOfBucket: Int): mutable.Buffer[mutable.Buffer[Int]] = {

    val sizeColumn = numberOfFeatures / numberOfBucket
    val r = numberOfFeatures % numberOfBucket
    val shuffledFeats = Random.shuffle(0 to numberOfFeatures - 1).toBuffer
    val columnsOfFeats = (0 until numberOfBucket).map( i => shuffledFeats.slice(sizeColumn * i, sizeColumn * (i + 1)) ).toBuffer
    shuffledFeats.takeRight(r).zipWithIndex.foreach{ case (f, idx) => columnsOfFeats(idx) += f }
    columnsOfFeats
  }
  /**
   *
   */
  final def prepareGsForRoughSet[O, T, V[A] <: GSimpleVector[A, V[A]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz], GS[X] <: GenSeq[X]](gs: GS[Sz[O, V[T]]]): GS[EasySupervizable[O, V[T]]] = {
    
    val occurPerFeat = obtainOccurencePerFeature(gs)

    val idxByValueByFeatIdx = obtainIdxByValueByFeatIdx(occurPerFeat)

    val learnableGs = gs.map{ sup => 
      val newWorkingVector = SupervizedVector(sup.v.vector.zipWithIndex.map{ case (value, idxF) => idxByValueByFeatIdx(idxF)(value) })
      EasySupervizable(sup.id, sup.o, sup.label, newWorkingVector)
    }

    learnableGs.asInstanceOf[GS[EasySupervizable[O, V[T]]]]
  }
  /**
   *
   */
  final def prepareGsForRoughSetHeuristic[O, T, V[A] <: GSimpleVector[A, V[A]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz], GS[X] <: GenSeq[X]](gs: GS[Sz[O, V[T]]], numberOfBucket: Int): (GS[EasySupervizable[O, V[T]]], mutable.Buffer[mutable.Buffer[Int]]) = {
    
    val bucketizedFeats = obtainRandomlyBucketizedFeatures(gs.head.v.vector.length, numberOfBucket)
    
    (
      prepareGsForRoughSet(gs).map( sup => sup.definedBucketizedFeatures(bucketizedFeats) ).asInstanceOf[GS[EasySupervizable[O, V[T]]]],
      bucketizedFeats
    )

  }
}