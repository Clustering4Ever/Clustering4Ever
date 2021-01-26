package org.clustering4ever.util

/**
 * @author Beck Gaël
 */
import org.apache.spark.rdd.RDD
import org.clustering4ever.clusteringtraits.ClusteringChaining
import org.clustering4ever.roottraits.VectorizationIDTypes._
import org.clustering4ever.roottraits.{ClusteringInformationsMapping, Clusterizable, GVector, VectorizationMapping}
import org.clustering4ever.sparkcoreextension._
import shapeless.HMap

import scala.collection.immutable
import scala.language.higherKinds
import scala.reflect.ClassTag
/**
 * This classe intend to run many distributed algorithms in Spark sequentially through a distributed system for large size datasets
 */
case class BigDataClusteringChaining[
    O,
    V <: GVector[V],
    Vecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, Vecto],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]
](
    final val data: RDD[Cz[O, V]],
    final val chainableID: Int,
    final val currentVectorization: Vecto[O, V],
    final val clusteringInformations: HMap[ClusteringInformationsMapping] = HMap.empty[ClusteringInformationsMapping]
)(implicit final val ct: ClassTag[Cz[O, V]]) extends ClusteringChaining[O, V, Cz, Vecto[O, V], RDD] {
    /**
     *
     */
    private final implicit val currentVectorizationMapping = currentVectorization.vectoMapping
    /**
     *
     */
    private final implicit val currentClusteringInformationMapping = ClusteringInformationsMapping[VectorizationID, ClusteringInformationsDistributed[O, V, Vecto]]
    /**
     * HMap of original and added EasyVectorizationDistributed
     */
    val vectorizations: HMap[VectorizationMapping] = HMap[VectorizationMapping](currentVectorization.vectorizationID -> currentVectorization)
    /**
     * A securty value in order to allow proper reduce of Chaining models
     */
    protected[clustering4ever] val fusionChainableSecurity: Int = 0
    /**
     *
     */
    final type Self[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, OtherVecto]] = BigDataClusteringChaining[O, GV, OtherVecto, Cz]
    /**
     *
     */
    protected[clustering4ever] final def fusionChainable(ccl: Self[V, Vecto]): Self[V, Vecto] = {

        val newFusionSecurity = fusionChainableSecurity + ccl.fusionChainableSecurity
        val updatedRunNumber = scala.math.max(clusteringRunNumber, ccl.clusteringRunNumber)
        val aVecto = vectorizations.get(currentVectorization.vectorizationID).get
        val bVecto = ccl.vectorizations.get(currentVectorization.vectorizationID).get
        val updatedCurrentVectorization = currentVectorization.updateClustering(aVecto.clusteringNumbers.toSeq ++ bVecto.clusteringNumbers:_*)
        val updatedVectorizations = vectorizations + ((updatedCurrentVectorization.vectorizationID, updatedCurrentVectorization))
        // To handle with clustering indices
        val aInfos = clusteringInformations.get(updatedCurrentVectorization.vectorizationID).get
        val bClusteringInfos = ccl.clusteringInformations.get(updatedCurrentVectorization.vectorizationID).get.clusteringInformations
        val updatedInfos = clusteringInformations + ((updatedCurrentVectorization.vectorizationID, aInfos.copy(clusteringInformations = aInfos.clusteringInformations ++ bClusteringInfos)))

        new BigDataClusteringChaining(
            data.zip(ccl.data).map{ case (cz1, cz2) => cz1.addClusterIDs(cz2.clusterIDs.takeRight(ccl.fusionChainableSecurity):_*) },
            chainableID,
            updatedCurrentVectorization,
            updatedInfos
        ) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedRunNumber
            override protected[clustering4ever] val fusionChainableSecurity = newFusionSecurity
        }

    }
    /**
     * Run one algorithm defined by user
     */
    final def runAlgorithm(algorithm: ClusteringAlgorithmDistributed[V, _ <: ClusteringModelDistributed[V]]): Self[V, Vecto] = {
        val model = algorithm.fit(data)
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
                updatedCurrentVectorization.vectorizationID -> ClusteringInformationsDistributed(
                    immutable.HashSet((updatedRunNumber, updatedCurrentVectorization, model))
                )
            )
        new BigDataClusteringChaining(updatedData, chainableID, updatedCurrentVectorization, updatedInfos) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedRunNumber
            override protected[clustering4ever] val fusionChainableSecurity = 1
        }

    }
    /**
     * Run multiples algorithms defined by user
     */
    final def runAlgorithms(algorithms: ClusteringAlgorithmDistributed[V, _ <: ClusteringModelDistributed[V]]*): Self[V, Vecto] = {

        val updatedFusionChainableSecurity = fusionChainableSecurity
        val updatedVectorizations = vectorizations

        algorithms.view.zipWithIndex.map{ case (algorithm, algIdx) =>

            val updatedRunNumber = clusteringRunNumber + algIdx

            (new BigDataClusteringChaining(data, chainableID, currentVectorization, clusteringInformations) {
                override val vectorizations = updatedVectorizations
                override val clusteringRunNumber = updatedRunNumber
                override protected[clustering4ever] val fusionChainableSecurity = updatedFusionChainableSecurity
            }).runAlgorithm(algorithm)

        }.reduce(_.fusionChainable(_))

    }
    /**
     *
     */
    private final def internalUpdating[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, OtherVecto]](vectorization: OtherVecto[O, GV]) = {
        val updatedRunNumber = clusteringRunNumber
        val updatedVectorizations = vectorizations.+((vectorization.vectorizationID, vectorization))(vectorization.vectoMapping)
        val updatedFusionChainableSecurity = fusionChainableSecurity
        (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity)
    }
    /**
     * Update working vector with given vectorization without saving neither previous working vector and new vector in clusterizable vectorized field to prevent clusterizable to becomes heavier and then slowing down clustering algorithms
     */
    final def updateVectorization[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, OtherVecto]](vectorization: OtherVecto[O, GV])(implicit ct: ClassTag[Cz[O, GV]]): Self[GV, OtherVecto] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new BigDataClusteringChaining(
            data.map(_.updateVectorization(vectorization)),
            chainableID,
            vectorization,
            clusteringInformations
        ) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedRunNumber
            override protected[clustering4ever] val fusionChainableSecurity = updatedFusionChainableSecurity
        }
    }
    /**
     * Add a vectorization in ClusteringChaining object which can be applied latter
     */
    final def addVectorizationOnData[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, OtherVecto]](vectorization: OtherVecto[O, GV]): Self[V, Vecto] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new BigDataClusteringChaining(
            data.map(_.addVectorization(vectorization)),
            chainableID,
            currentVectorization,
            clusteringInformations
        ) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedRunNumber
            override protected[clustering4ever] val fusionChainableSecurity = updatedFusionChainableSecurity
        }

    }
    /**
     * Update the current working vector for another existing vector in clusterizable vectorized field
     */
    final def switchForExistingVectorization[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, OtherVecto]](vectorization: OtherVecto[O, GV])(implicit ct: ClassTag[Cz[O, GV]]): Self[GV, OtherVecto] = {
        val updatedVectorData = data.map(_.switchForExistingVectorization(vectorization))
        val updatedCurrentVectorization = vectorizations.get(vectorization.vectorizationID)(vectorization.vectoMapping).get
        val updatedGlobalRunNumber = clusteringRunNumber
        val fusionChainableSecurityCopy = fusionChainableSecurity
        val updatedVectorizations = vectorizations
        new BigDataClusteringChaining(updatedVectorData, chainableID, updatedCurrentVectorization, clusteringInformations) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedGlobalRunNumber
            override protected[clustering4ever] val fusionChainableSecurity = fusionChainableSecurityCopy
        }
    }

}
/**
 *
 */
object BigDataClusteringChaining extends Serializable {
    final def apply[O, V <: GVector[V], Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]) = {
        new BigDataClusteringChaining(
            data,
            scala.util.Random.nextInt,
            EasyVectorizationDistributed[O, V](0)
        )
    }
}