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
import org.clustering4ever.clustering.{ClusteringArgsLocal, ClusteringModelLocal}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.vectorizations.VectorizationLocal
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.extensibleAlgorithmNature._
import org.clustering4ever.clustering.{ClusteringInformationsLocal, ClusteringAlgorithmLocal, DataExplorator, AlgorithmsRestrictions}
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.vectorizations.EasyVectorizationLocal
import scala.annotation.meta.param
/**
 * This classe intend to run many algorithms parallely on a distributed system for medium size datasets
 */
case class DistributedClusteringChaining[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Vecto <: VectorizationLocal[O, V, Vecto],
    S[X] <: Seq[X]
](
    @transient val sc: SparkContext,
    val data: S[Cz[ID, O, V]],
    /**
     * The ID of this clustering chainable, it stays constant over algorithms launch
     */
    val chainableID: Int,
    val currentVectorization: Vecto,
    val clusteringInformations: HMap[ClusteringInformationsMapping] = HMap.empty[ClusteringInformationsMapping]
)(implicit ct: ClassTag[Cz[ID, O, V]], ct2: ClassTag[S[Cz[ID, O, V]]]) extends DataExplorator[ID, O, V, Cz, S] with AlgorithmsRestrictions[ID, O, V, Cz, S] {
    /**
     *
     */
    protected implicit val currentVectorizationMapping = currentVectorization.vectoMapping
    /**
     *
     */
    protected implicit val currentClusteringInformationMapping = ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[ID, O, V, Cz, Vecto, S]]
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
    val globalClusteringRunNumber: Int = -1
    /**
     *
     */
    type Self[GV <: GVector[GV], OtherVecto <: VectorizationLocal[O, GV, OtherVecto]] = DistributedClusteringChaining[ID, O, GV, Cz, OtherVecto, S]
    /**
     *
     */
    type AlgorithmsRestrictions[GV <: GVector[GV]] = ClusteringAlgorithmLocal[ID, O, GV, Cz, S, ClusteringArgsLocal[GV], ClusteringModelLocal[ID, O, GV, Cz, S, ClusteringArgsLocal[GV]]]
    /**
     * Run one algorithm defined by user
     */
    def runAlgorithm(algorithm: AlgorithmsRestrictions[V]): Self[V, Vecto] = {
        val model = algorithm.run(data)
        val updatedRunNumber = globalClusteringRunNumber + 1
        val updatedData = model.obtainClustering(data)
        val updatedCurrentVectorization = currentVectorization.updateClustering(updatedRunNumber)
        val updatedVectorizations = vectorizations + ((updatedCurrentVectorization.vectorizationID, updatedCurrentVectorization))
        val aInfos = clusteringInformations.get(updatedCurrentVectorization.vectorizationID)
        val updatedInfos = if(aInfos.isDefined) clusteringInformations + (
                updatedCurrentVectorization.vectorizationID,
                aInfos.get.copy(clusteringInformations = aInfos.get.clusteringInformations + ((updatedRunNumber, updatedCurrentVectorization, algorithm.args, model)))
            )
            else HMap[ClusteringInformationsMapping](
                updatedCurrentVectorization.vectorizationID -> new ClusteringInformationsLocal(immutable.HashSet((updatedRunNumber, updatedCurrentVectorization, algorithm.args, model)))
            )
        new DistributedClusteringChaining(sc, updatedData, chainableID, updatedCurrentVectorization, updatedInfos) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = 1
        }
    }
    /**
     *
     */
    def runAlgorithms(defaultParallelism: Int, algorithms: AlgorithmsRestrictions[V]*): Self[V, Vecto] = {

        val dataBrodcasted = sc.broadcast(data)
        val clusteringChainingLocalFinal@ClusteringChainingLocal(dataFinal, chainableIDFinal, currentVectorizationFinal, clusteringInformationsFinal) = sc.parallelize(algorithms.zipWithIndex, defaultParallelism).map{ case (algo, algIdx) =>
            val updatedFusionChainableSecurity = fusionChainableSecurity
            val updatedRunNumber = globalClusteringRunNumber + algIdx
            val updateVectorization = vectorizations
            (new ClusteringChainingLocal(dataBrodcasted.value, chainableID, currentVectorization, clusteringInformations) {
                override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
                override val globalClusteringRunNumber = updatedRunNumber
                override val vectorizations = updateVectorization
            }).runAlgorithm(algo)
        }.reduce(_.fusionChainable(_))

        new DistributedClusteringChaining(sc, dataFinal, chainableIDFinal, currentVectorizationFinal, clusteringInformationsFinal) {
            override val vectorizations = clusteringChainingLocalFinal.vectorizations
            override val globalClusteringRunNumber = clusteringChainingLocalFinal.globalClusteringRunNumber
            override protected[chaining] val fusionChainableSecurity = clusteringChainingLocalFinal.fusionChainableSecurity
        }
    }
    /**
     *
     */
    private def internalUpdating[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, GV]) = {
        val updatedRunNumber = globalClusteringRunNumber
        val updatedVectorizations = vectorizations.+((vectorization.vectorizationID, vectorization))(vectorization.vectoMapping)
        val updatedFusionChainableSecurity = fusionChainableSecurity
        (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity)
    }
    /**
     *
     */
    def addVectorizationOnData[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, GV]): Self[V, Vecto] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new DistributedClusteringChaining(
            sc,
            data.map(_.addVectorization(vectorization)).asInstanceOf[S[Cz[ID, O, V]]],
            chainableID,
            currentVectorization,
            clusteringInformations
        ) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
        }

    }
    /**
     * Update the current vector for another
     */
    def switchToAnotherExistingVector[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto[A, B]]] (vectorization: OtherVecto[O, GV])(implicit ct: ClassTag[Cz[ID, O, GV]], ct2: ClassTag[S[Cz[ID, O, GV]]]): Self[GV, OtherVecto[O, GV]] = {

        val updatedVectorData = data.map(_.switchForExistingVector(vectorization)).asInstanceOf[S[Cz[ID, O, GV]]]
        val updatedCurrentVectorization = vectorizations.get(vectorization.vectorizationID)(vectorization.vectoMapping).get
        val updatedGlobalRunNumber = globalClusteringRunNumber
        val fusionChainableSecurityCopy = fusionChainableSecurity
        val updatedVectorizations = vectorizations

        new DistributedClusteringChaining(sc, updatedVectorData, chainableID, updatedCurrentVectorization, clusteringInformations) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedGlobalRunNumber
            override protected[chaining] val fusionChainableSecurity = fusionChainableSecurityCopy
        }
    }
}
/**
 *
 */
object DistributedClusteringChaining extends Serializable {
    def apply[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], S[X] <: Seq[X]](@(transient @param) sc: SparkContext, data: S[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]], ct2: ClassTag[S[Cz[ID, O, V]]]) = {
        new DistributedClusteringChaining(
            sc,
            data,
            scala.util.Random.nextInt,
            EasyVectorizationLocal[O, V](0)
        )
    }
}