package org.clustering4ever.scala.preprocessing.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{mutable, immutable, GenSeq}
import org.clustering4ever.supervizables.Supervizable
import scala.util.Random
import org.clustering4ever.vectorizables.Vectorizable
import org.clustering4ever.supervizables.EasySupervizable
import org.clustering4ever.vectors.{GVector, GSimpleVector, SupervizedVector}
/**
 * Theses functions are used to preprocess raw data                    
 */
object Util extends Serializable {
  /**
   * Determine in which interval falls a value given a specific range.
   * @return i if smaller or equal than ith value (starting at 0) and range.size +1 if bigger than the last range value 
   */
  final def whichInterval[N](d: N, range: Seq[N])(implicit num: Numeric[N]): Int  = {
    @annotation.tailrec
    def go(i: Int): Int = {
      if(num.lteq(d, range(i))) i
      else if(num.gt(d, range.last)) range.size + 1
      else go(i + 1)
    }
    go(0)
  }
  /**
   *
   */
  private final def obtainIdxByValueByFeatIdx[T](occurPerFeat: Seq[mutable.HashSet[T]]) = { 
    immutable.HashMap(
      occurPerFeat.map( values => immutable.HashMap(values.toSeq.zipWithIndex:_*) )
      .zipWithIndex.map(_.swap)
    :_*)
  }
  /**
   *
   */
  private final def reductHashSeq[T](s1: Seq[mutable.HashSet[T]], s2: Seq[mutable.HashSet[T]]) = s1.zip(s2).map( x => x._1 ++ x._2 )
  /**
   *
   */
  private final def reduceOccFeaturesGs[T](gs: GenSeq[Seq[mutable.HashSet[T]]]) = gs.reduce(reductHashSeq(_, _))
  /**
   *
   */
  final def obtainOccurencePerFeature[O, T, S[X] <: Seq[X], V[A, B[X] <: Seq[X]] <: GSimpleVector[A, B[A], V[A, B]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](gs: GenSeq[Sz[O, V[T, S]]]): Seq[mutable.HashSet[T]] = reduceOccFeaturesGs(gs.map(_.v.vector.map(mutable.HashSet(_))))
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
  final def prepareGsForRoughSet[O, T, S[X] <: Seq[X], V[A, B[X] <: Seq[X]] <: GSimpleVector[A, B[A], V[A, B]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz], GS[X] <: GenSeq[X]](gs: GS[Sz[O, V[T, S]]]): GS[EasySupervizable[O, V[T, S]]] = {
    
    val occurPerFeat = obtainOccurencePerFeature(gs)

    val idxByValueByFeatIdx = obtainIdxByValueByFeatIdx(occurPerFeat)

    val learnableGs = gs.map{ sup => 
      val newWorkingVector = SupervizedVector(sup.v.vector.zipWithIndex.map{ case (value, idxF) => idxByValueByFeatIdx(idxF)(value) }.asInstanceOf[S[T]])
      EasySupervizable(sup.id, sup.o, sup.label, newWorkingVector)
    }

    learnableGs.asInstanceOf[GS[EasySupervizable[O, V[T, S]]]]
  }
  /**
   *
   */
  final def prepareGsForRoughSetHeuristic[O, T, S[X] <: Seq[X], V[A, B[X] <: Seq[X]] <: GSimpleVector[A, B[A], V[A, B]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz], GS[X] <: GenSeq[X]](gs: GS[Sz[O, V[T, S]]], numberOfBucket: Int): (GS[EasySupervizable[O, V[T, S]]], mutable.Buffer[mutable.Buffer[Int]]) = {
    
    val bucketizedFeats = obtainRandomlyBucketizedFeatures(gs.head.v.vector.size, numberOfBucket)
    
    (
      prepareGsForRoughSet(gs).map( sup => sup.definedBucketizedFeatures(bucketizedFeats) ).asInstanceOf[GS[EasySupervizable[O, V[T, S]]]],
      bucketizedFeats
    )

  }
}