package org.clustering4ever.clustering.chaining
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import shapeless.HMap
import org.apache.spark.SparkContext
import scala.collection.{mutable, immutable, GenSeq}
import scala.collection.parallel.immutable.ParSeq
import org.apache.spark.rdd.RDD
import org.clustering4ever.clustering.ClusteringModelLocal
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.vectorizations.VectorizationLocal
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.extensibleAlgorithmNature._
import org.clustering4ever.clustering.{ClusteringInformationsLocal, ClusteringAlgorithmLocal, DataExplorator}
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.vectorizations.EasyVectorizationLocal
import scala.annotation.meta.param
/**
 * This classe intend to run many algorithms parallely on a distributed system for medium size datasets
 */
case class DistributedClusteringChaining[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Vecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, Vecto],
    S[X] <: Seq[X]
](
    @transient final val sc: SparkContext,
    final val data: S[Cz[O, V]],
    final val chainableID: Int,
    final val currentVectorization: Vecto[O, V],
    final val clusteringInformations: HMap[ClusteringInformationsMapping] = HMap.empty[ClusteringInformationsMapping]
)(implicit ct: ClassTag[Cz[O, V]], ct2: ClassTag[S[Cz[O, V]]]) extends DataExplorator[O, V, Cz, S] {
    /**
     *
     */
    protected implicit val currentVectorizationMapping = currentVectorization.vectoMapping
    /**
     *
     */
    protected implicit val currentClusteringInformationMapping = ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[O, V, Vecto]]
    /**
     * HMap of original and added EasyVectorizationDistributed
     */
    val vectorizations: HMap[VectorizationMapping] = HMap[VectorizationMapping](currentVectorization.vectorizationID -> currentVectorization)
    /**
     * A securty value in order to allow proper reduce of Chaining models
     */
    protected val fusionChainableSecurity: Int = 0
    /**
     *
     */
    val clusteringRunNumber: Int = -1
    /**
     *
     */
    final type Self[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]] = DistributedClusteringChaining[O, GV, Cz, OtherVecto, S]
    /**
     * Run one algorithm defined by user
     */
    final def runAlgorithm(algorithm: ClusteringAlgorithmLocal[V, _ <: ClusteringModelLocal[V]]): Self[V, Vecto] = {
        val model = algorithm.run(data)
        val updatedRunNumber = clusteringRunNumber + 1
        val updatedData = model.obtainClustering(data)
        val updatedCurrentVectorization = currentVectorization.updateClustering(updatedRunNumber)
        val updatedVectorizations = vectorizations + ((updatedCurrentVectorization.vectorizationID, updatedCurrentVectorization))
        val aInfos = clusteringInformations.get(updatedCurrentVectorization.vectorizationID)
        val updatedInfos = if(aInfos.isDefined) clusteringInformations + (
                updatedCurrentVectorization.vectorizationID,
                aInfos.get.copy(clusteringInformations = aInfos.get.clusteringInformations + ((updatedRunNumber, updatedCurrentVectorization, model)))
            )
            else HMap[ClusteringInformationsMapping](
                updatedCurrentVectorization.vectorizationID -> ClusteringInformationsLocal(immutable.HashSet((updatedRunNumber, updatedCurrentVectorization, model)))
            )
        new DistributedClusteringChaining(sc, updatedData, chainableID, updatedCurrentVectorization, updatedInfos) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = 1
        }
    }
    /**
     *
     */
    final def runAlgorithms(defaultParallelism: Int, argsModels: ClusteringAlgorithmLocal[V, _ <: ClusteringModelLocal[V]]*): Self[V, Vecto] = {

        val dataBrodcasted = sc.broadcast(data)
        val clusteringChainingLocalFinal@ClusteringChainingLocal(dataFinal, chainableIDFinal, currentVectorizationFinal, clusteringInformationsFinal) = sc.parallelize(argsModels.zipWithIndex, defaultParallelism).map{ case (algorithm, algIdx) =>
            val updatedFusionChainableSecurity = fusionChainableSecurity
            val updatedRunNumber = clusteringRunNumber + algIdx
            val updatedVectorization = vectorizations
            (new ClusteringChainingLocal(dataBrodcasted.value, chainableID, currentVectorization, clusteringInformations) {
                override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
                override val clusteringRunNumber = updatedRunNumber
                override val vectorizations = updatedVectorization
            }).runAlgorithm(algorithm)
        }.reduce(_.fusionChainable(_))

        new DistributedClusteringChaining(sc, dataFinal, chainableIDFinal, currentVectorizationFinal, clusteringInformationsFinal) {
            override val vectorizations = clusteringChainingLocalFinal.vectorizations
            override val clusteringRunNumber = clusteringChainingLocalFinal.clusteringRunNumber
            override protected[chaining] val fusionChainableSecurity = clusteringChainingLocalFinal.fusionChainableSecurity
        }
    }
    /**
     *
     */
    final private def internalUpdating[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, GV]) = {
        val updatedRunNumber = clusteringRunNumber
        val updatedVectorizations = vectorizations.+((vectorization.vectorizationID, vectorization))(vectorization.vectoMapping)
        val updatedFusionChainableSecurity = fusionChainableSecurity
        (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity)
    }
    /**
     *
     */
    final def addVectorizationOnData[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, GV]): Self[V, Vecto] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new DistributedClusteringChaining(
            sc,
            data.map(_.addVectorization(vectorization)).asInstanceOf[S[Cz[O, V]]],
            chainableID,
            currentVectorization,
            clusteringInformations
        ) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
        }

    }
    /**
     * Update the current working vector for another existing vector in clusterizable vectorized field
     */
    final def switchForExistingVectorization[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]] (vectorization: OtherVecto[O, GV])(implicit ct: ClassTag[Cz[O, GV]], ct2: ClassTag[S[Cz[O, GV]]]): Self[GV, OtherVecto] = {

        val updatedVectorData = data.map(_.switchForExistingVectorization(vectorization)).asInstanceOf[S[Cz[O, GV]]]
        val updatedCurrentVectorization = vectorizations.get(vectorization.vectorizationID)(vectorization.vectoMapping).get
        val updatedGlobalRunNumber = clusteringRunNumber
        val fusionChainableSecurityCopy = fusionChainableSecurity
        val updatedVectorizations = vectorizations

        new DistributedClusteringChaining(sc, updatedVectorData, chainableID, updatedCurrentVectorization, clusteringInformations) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedGlobalRunNumber
            override protected[chaining] val fusionChainableSecurity = fusionChainableSecurityCopy
        }
    }
}
/**
 *
 */
object DistributedClusteringChaining extends Serializable {
    final def apply[O, V <: GVector[V], Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], S[X] <: Seq[X]](@(transient @param) sc: SparkContext, data: S[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]], ct2: ClassTag[S[Cz[O, V]]]) = {
        new DistributedClusteringChaining(
            sc,
            data,
            scala.util.Random.nextInt,
            EasyVectorizationLocal[O, V](0)
        )
    }
}