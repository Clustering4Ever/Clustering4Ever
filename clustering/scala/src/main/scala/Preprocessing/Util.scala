package org.clustering4ever.scala.preprocessing.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{mutable, immutable, GenSeq}
import org.clustering4ever.preprocessing.{DFCL, HDFCL}
import scala.util.Random
import org.clustering4ever.scala.vectorizables.Vector
/**
 * Theses functions are used to preprocess raw data                    
 */
object Util extends Serializable {
  /**
   * Determine in which interval falls a value given a specific range.
   * @return i if smaller or equal than ith value (starting at 0) and range.size +1 if bigger than the last range value 
   */
  def whichInterval[N](d: N, range: Seq[N])(implicit num: Numeric[N])  = {
    @annotation.tailrec
    def go(i: Int): Int = {
      if( num.lteq(d, range(i)) ) i
      else if( num.gt(d, range.last) ) range.size + 1
      else go(i + 1)
    }
    go(0)
  }
  private def obtainIdxByValueByFeatIdx[T](occurPerFeat: Seq[mutable.HashSet[T]]) = { 
    immutable.HashMap(
      occurPerFeat.map( values => immutable.HashMap(values.toSeq.zipWithIndex:_*) )
      .zipWithIndex.map(_.swap)
    :_*)
  }
  /**
   *
   */
  private def reductHashSeq[T](s1: Seq[mutable.HashSet[T]], s2: Seq[mutable.HashSet[T]]) = s1.zip(s2).map( x => x._1 ++ x._2 )
  /**
   *
   */
  private def reduceOccFeaturesGs[T](gs: GenSeq[Seq[mutable.HashSet[T]]]) = gs.reduce(reductHashSeq(_, _))
  /**
   *
   */
  def obtainOccurencePerFeature[ID: Numeric, T, V[T] <: Seq[T]](gs: GenSeq[DFCL[ID, V[T]]]): Seq[mutable.HashSet[T]] = reduceOccFeaturesGs(gs.map(_.originalVector.map(mutable.HashSet(_))))
  /**
   *
   */
  def obtainRandomlyBucketizedFeatures(numberOfFeatures: Int, numberOfBucket: Int): mutable.Buffer[mutable.Buffer[Int]] = {

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
  def prepareGsForRoughSet[ID: Numeric, T, V[X] <: Seq[X]](gs: GenSeq[DFCL[ID, V[T]]]): GenSeq[DFCL[ID, V[T]]] = {
    
    val occurPerFeat = obtainOccurencePerFeature(gs)

    val idxByValueByFeatIdx = obtainIdxByValueByFeatIdx(occurPerFeat)

    val learnableGs = gs.map( dfcl => new DFCL(dfcl.id, new Vector(dfcl.originalVector.zipWithIndex.map{ case (value, idxF) => idxByValueByFeatIdx(idxF)(value) }.asInstanceOf[V[T]]), dfcl.label) )

    learnableGs
  }
  /**
   *
   */
  def prepareGsForRoughSetHeuristic[ID: Numeric, T, V[X] <: Seq[X]](gs: GenSeq[DFCL[ID, V[T]]], numberOfBucket: Int): (GenSeq[HDFCL[ID, Int, mutable.Buffer, mutable.Buffer]], mutable.Buffer[mutable.Buffer[Int]]) = {
    
    val bucketizedFeats = obtainRandomlyBucketizedFeatures(gs.head.originalVector.size, numberOfBucket)
    
    (
      prepareGsForRoughSet(gs).map( dfcl => dfcl.bucketizedFeatures(bucketizedFeats) ),
      bucketizedFeats
    )

  }
}