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
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.vectorizations.{VectorizationDistributed, EasyVectorizationDistributed}
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.extensibleAlgorithmNature._
import org.clustering4ever.clustering.ClusteringChaining
import org.clustering4ever.clustering.rdd.{ClusteringArgsDistributed, ClusteringModelDistributed, ClusteringAlgorithmDistributed, ClusteringInformationsDistributed}
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
/**
 * This classe intend to run many distributed algorithms in Spark sequentially through a distributed system for large size datasets
 */
case class BigDataClusteringChaining[
    ID,
    O,
    V <: GVector[V],
    Vecto <: VectorizationDistributed[O, V, Vecto],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]
](
    val data: RDD[Cz[ID, O, V]],
    val chainableID: Int,
    val currentVectorization: Vecto,
    val clusteringInformations: HMap[ClusteringInformationsMapping] = HMap.empty[ClusteringInformationsMapping]
)(implicit val ct: ClassTag[Cz[ID, O, V]]) extends ClusteringChaining[ID, O, V, Cz, Vecto, RDD] {
    /**
     *
     */
    private implicit val currentVectorizationMapping = currentVectorization.vectoMapping
    /**
     *
     */
    private implicit val currentClusteringInformationMapping = ClusteringInformationsMapping[VectorizationID, ClusteringInformationsDistributed[ID, O, V, Cz, Vecto]]
    /**
     * HMap of original and added EasyVectorizationDistributed
     */
    val vectorizations: HMap[VectorizationMapping] = HMap[VectorizationMapping](currentVectorization.vectorizationID -> currentVectorization)
    /**
     * A securty value in order to allow proper reduce of Chaining models
     */
    protected[chaining] val fusionChainableSecurity: Int = 0
    /**
     *
     */
    type Self[GV <: GVector[GV], OtherVecto <: VectorizationDistributed[O, GV, OtherVecto]] = BigDataClusteringChaining[ID, O, GV, OtherVecto, Cz]
    /**
     *
     */
    type AlgorithmsRestrictions[GV <: GVector[GV]] = ClusteringAlgorithmDistributed[ID, O, GV, Cz, ClusteringArgsDistributed[GV], ClusteringModelDistributed[ID, O, GV, Cz, ClusteringArgsDistributed[GV]]]
    /**
     *
     */
    protected[chaining] def fusionChainable(ccl: Self[V, Vecto]): Self[V, Vecto] = {

        val newFusionSecurity = fusionChainableSecurity + ccl.fusionChainableSecurity
        val updatedRunNumber = scala.math.max(globalClusteringRunNumber, ccl.globalClusteringRunNumber)
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
            override val globalClusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = newFusionSecurity
        }

    }
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
                updatedCurrentVectorization.vectorizationID -> ClusteringInformationsDistributed(
                    immutable.HashSet((updatedRunNumber, updatedCurrentVectorization, algorithm.args, model))
                )
            )
        new BigDataClusteringChaining(updatedData, chainableID, updatedCurrentVectorization, updatedInfos) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = 1
        }

    }
    /**
     * Run multiples algorithms defined by user
     */
    def runAlgorithms(algorithms: AlgorithmsRestrictions[V]*): Self[V, Vecto] = {

        val updatedFusionChainableSecurity = fusionChainableSecurity
        val updatedVectorizations = vectorizations

        algorithms.view.zipWithIndex.map{ case (alg, algIdx) =>

            val updatedRunNumber = globalClusteringRunNumber + algIdx

            (new BigDataClusteringChaining(data, chainableID, currentVectorization, clusteringInformations) {
                override val vectorizations = updatedVectorizations
                override val globalClusteringRunNumber = updatedRunNumber
                override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
            }).runAlgorithm(alg)

        }.reduce(_.fusionChainable(_))

    }
    /**
     *
     */
    private def internalUpdating[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, GV]) = {
        val updatedRunNumber = globalClusteringRunNumber
        val updatedVectorizations = vectorizations.+((vectorization.vectorizationID, vectorization))(vectorization.vectoMapping)
        val updatedFusionChainableSecurity = fusionChainableSecurity
        (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity)
    }
    /**
     *
     */
    def updateVectorizationOnData[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, GV])(implicit ct: ClassTag[Cz[ID, O, GV]]): Self[GV, OtherVecto[O, GV]] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new BigDataClusteringChaining(
            data.map(_.updateVectorization(vectorization)),
            chainableID,
            vectorization,
            clusteringInformations
        ) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
        }
    }
    /**
     *
     */
    def addVectorizationOnData[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, GV]): Self[V, Vecto] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new BigDataClusteringChaining(
            data.map(_.addVectorization(vectorization)),
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
    def switchToAnotherExistingVector[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, GV])(implicit ct: ClassTag[Cz[ID, O, GV]]): Self[GV, OtherVecto[O, GV]] = {
        val updatedVectorData = data.map(_.switchForExistingVector(vectorization))
        val updatedCurrentVectorization = vectorizations.get(vectorization.vectorizationID)(vectorization.vectoMapping).get
        val updatedGlobalRunNumber = globalClusteringRunNumber
        val fusionChainableSecurityCopy = fusionChainableSecurity
        val updatedVectorizations = vectorizations
        new BigDataClusteringChaining(updatedVectorData, chainableID, updatedCurrentVectorization, clusteringInformations) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedGlobalRunNumber
            override protected[chaining] val fusionChainableSecurity = fusionChainableSecurityCopy
        }
    }
    /**
     *
     */
    // def runAlgorithmsOnMultipleVectorizations[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, OtherVecto]](
    //     vectorizations: Seq[OtherVecto],
    //     algorithms: AlgorithmsRestrictions[V]*
    // ): Self[V, OtherVecto] = {
    //     vectorizations.view.map( vectorization => switchToAnotherExistingVector(vectorization).runAlgorithms(algorithms:_*) ).reduce(_.fusionChainable(_))
    // }
}
/**
 *
 */
object BigDataClusteringChaining extends Serializable {
    def apply[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]) = {
        new BigDataClusteringChaining(
            data,
            scala.util.Random.nextInt,
            EasyVectorizationDistributed[O, V](0)
        )
    }
}