package org.clustering4ever.clustering.chaining
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{GenSeq, mutable, immutable}
import shapeless.{HMap, HNil}
import org.clustering4ever.clustering.ClusteringModelLocal
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.extensibleAlgorithmNature._
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.clustering.kcenters.scala.KCenters
import org.clustering4ever.clustering.{ClusteringChaining, ClusteringAlgorithm, ClusteringAlgorithmLocal, ClusteringInformationsLocal}
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.enums.ClusteringIndices
import org.clustering4ever.enums.ExternalsIndices._
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.vectorizations.{VectorizationLocal, EasyVectorizationLocal}
/**
 * This classe intend to run many algorithms parallely on a local system for medium size datasets
 * @tparam ID the identifier of data points
 */
case class ClusteringChainingLocal[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Vecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, Vecto],
    GS[X] <: GenSeq[X]
](
    val data: GS[Cz[ID, O, V]],
    val chainableID: Int,
    val currentVectorization: Vecto[O, V],
    val clusteringInformations: HMap[ClusteringInformationsMapping] = HMap.empty[ClusteringInformationsMapping]
)(implicit val ct: ClassTag[Cz[ID, O, V]]) extends ClusteringChaining[ID, O, V, Cz, Vecto[O, V], GS] {
    /**
     *
     */
    private implicit val currentVectorizationMapping = currentVectorization.vectoMapping
    /**
     *
     */
    private implicit val currentClusteringInformationMapping = ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[O, V, Vecto]]

    val vectorizations: HMap[VectorizationMapping] = HMap[VectorizationMapping](currentVectorization.vectorizationID -> currentVectorization)

    protected[chaining] val fusionChainableSecurity: Int = 0
    /**
     * ClusteringChainingLocal type with a specific GVector and Vectorization
     */
    type Self[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]] = ClusteringChainingLocal[ID, O, GV, Cz, OtherVecto, GS]
    /**
     *
     */
    protected[chaining] def fusionChainable(ccl: Self[V, Vecto]): Self[V, Vecto] = {
        val newFusionSecurity = fusionChainableSecurity + ccl.fusionChainableSecurity
        val updatedRunNumber = scala.math.max(clusteringRunNumber, ccl.clusteringRunNumber)

        // Handle case of different vectorization
        // if(currentVectorization.vectorizationID == ccl.currentVectorization.vectorizationID)

        val aVecto = vectorizations.get(currentVectorization.vectorizationID).get
        val bVecto = ccl.vectorizations.get(currentVectorization.vectorizationID).get
        val updatedCurrentVectorization = currentVectorization.updateClustering(aVecto.clusteringNumbers.union(bVecto.clusteringNumbers).toSeq:_*)
        val updatedVectorizations = vectorizations + ((updatedCurrentVectorization.vectorizationID, updatedCurrentVectorization))
        // To handle with clustering indices
        val aInfos = clusteringInformations.get(updatedCurrentVectorization.vectorizationID).get
        val bClusteringInfos = ccl.clusteringInformations.get(updatedCurrentVectorization.vectorizationID).get.clusteringInformations
        val updatedInfos = clusteringInformations + ((updatedCurrentVectorization.vectorizationID, aInfos.copy(clusteringInformations = aInfos.clusteringInformations ++ bClusteringInfos)))

        new ClusteringChainingLocal(
            data.zip(ccl.data).map{ case (cz1, cz2) => cz1.addClusterIDs(cz2.clusterIDs.takeRight(ccl.fusionChainableSecurity):_*) }.asInstanceOf[GS[Cz[ID, O, V]]],
            chainableID,
            updatedCurrentVectorization,
            updatedInfos
        ) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = newFusionSecurity
        }
    }
    /**
     * Run one algorithm defined by user
     */
    def runAlgorithm(algorithm: ClusteringAlgorithmLocal[V, _ <: ClusteringModelLocal[V]]): Self[V, Vecto] = {
        val model = algorithm.run(data)
        val updatedRunNumber = clusteringRunNumber + 1
        val updatedData = model.obtainClustering(data)
        val updatedCurrentVectorization = currentVectorization.updateClustering(updatedRunNumber)
        val updatedVectorizations = vectorizations + ((updatedCurrentVectorization.vectorizationID, updatedCurrentVectorization))
        val aInfos = clusteringInformations.get(updatedCurrentVectorization.vectorizationID)
        val updatedInfos = if(aInfos.isDefined) clusteringInformations + ((
                updatedCurrentVectorization.vectorizationID,
                aInfos.get.copy(clusteringInformations = aInfos.get.clusteringInformations + ((updatedRunNumber, updatedCurrentVectorization, model)))
            ))
            else HMap[ClusteringInformationsMapping](
                updatedCurrentVectorization.vectorizationID -> ClusteringInformationsLocal(immutable.HashSet((updatedRunNumber, updatedCurrentVectorization, model)))
            )
        new ClusteringChainingLocal(updatedData, chainableID, updatedCurrentVectorization, updatedInfos) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = 1
        }
    }
    /**
     * Run multiples algorithms defined by user
     */
    def runAlgorithms(algorithms: ClusteringAlgorithmLocal[V, _ <: ClusteringModelLocal[V]]*): Self[V, Vecto] = {

        val updatedFusionChainableSecurity = fusionChainableSecurity

        algorithms.par.zipWithIndex.map{ case (alg, algIdx) =>

            val updatedRunNumber = clusteringRunNumber + algIdx
            val updatedVectorizations = vectorizations
            (new ClusteringChainingLocal(data, chainableID, currentVectorization, clusteringInformations) {
                override val vectorizations = updatedVectorizations
                override val clusteringRunNumber = updatedRunNumber
                override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
            }).runAlgorithm(alg)

        }.reduce(_.fusionChainable(_))

    }
    /**
     * Does nothing for the moment, please be patient, better, help us :)
     * Optimize hyperparameter for a given algorithm probably using Bayensian Optimization
     */
    // def optimizeHyperparametersViaInternalIndices[
    //     CA <: ClusteringModelLocal[V],
    //     Algo <: ClusteringAlgorithmLocal[V, CA]
    // ](algorithms: Algo, metrics: Distance[V])(ci: InternalsIndicesType*) = {

    // }
    /**
     * Does nothing for the moment, please be patient, better, help us :)
     * Optimize hyperparameter for a given algorithm probably using Bayensian Optimization
     */
    // def optimizeHyperparametersViaExternalIndices[
    //     CA <: ClusteringModelLocal[V],
    //     Algo <: ClusteringAlgorithmLocal[V, CA]
    // ](algorithms: Algo, gt: GS[ClusterID], ci: ExternalsIndicesType*) = {

    // }
    /**
     *
     */
    private def internalUpdating[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, GV]) = {
        val updatedRunNumber = clusteringRunNumber
        val updatedVectorizations = vectorizations.+((vectorization.vectorizationID, vectorization))(vectorization.vectoMapping)
        val updatedFusionChainableSecurity = fusionChainableSecurity
        (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity)
    }
    /**
     * Update working vector with given vectorization without saving neither previous working vector and new vector in clusterizable vectorized field to prevent clusterizable to becomes heavier and then slowing down clustering algorithms
     */
    def updateVectorization[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, GV])(implicit ct: ClassTag[Cz[ID, O, GV]]): Self[GV, OtherVecto] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new ClusteringChainingLocal(
            data.map(_.updateVectorization(vectorization)).asInstanceOf[GS[Cz[ID, O, GV]]],
            chainableID,
            vectorization,
            clusteringInformations
        ) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
        }
    }
    /**
     * Add a vectorization in ClusteringChaining object which can be applied latter
     */
    def addVectorization[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, GV]): Self[V, Vecto] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new ClusteringChainingLocal(
            data,
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
     *
     */
    def addVectorizationOnData[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, GV]): Self[V, Vecto] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new ClusteringChainingLocal(
            data.map(_.addVectorization(vectorization)).asInstanceOf[GS[Cz[ID, O, V]]],
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
    def switchForExistingVectorization[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, GV])(implicit ct: ClassTag[Cz[ID, O, GV]]): Self[GV, OtherVecto] = {
        val updatedVectorData = data.map(_.switchForExistingVectorization(vectorization)).asInstanceOf[GS[Cz[ID, O, GV]]]
        val updatedCurrentVectorization = vectorizations.get(vectorization.vectorizationID)(VectorizationMapping[VectorizationID, OtherVecto[O, GV]]).get
        val updatedGlobalRunNumber = clusteringRunNumber
        val fusionChainableSecurityCopy = fusionChainableSecurity
        val updatedVectorizations = vectorizations
        new ClusteringChainingLocal(updatedVectorData, chainableID, updatedCurrentVectorization, clusteringInformations) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedGlobalRunNumber
            override protected[chaining] val fusionChainableSecurity = fusionChainableSecurityCopy
        }
    }
    /**
     *
     */
    // def runAlgorithmsOnMultipleVectorizationsOfSameNature[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](
    def runAlgorithmsOnMultipleVectorizationsOfSameNature[OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](
        vectorizations: Seq[OtherVecto[O, V]],
        algorithms: ClusteringAlgorithmLocal[V, _ <: ClusteringModelLocal[V]]*
    ): Self[V, OtherVecto] = {
        vectorizations.par.map( vectorization => switchForExistingVectorization(vectorization).runAlgorithms(algorithms:_*) ).reduce(_.fusionChainable(_))
    }
}
/**
 *
 */
object ClusteringChainingLocal extends Serializable {
    /**
     *
     */
    def apply[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]], chainingID: Int)(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingLocal[ID, O, V, Cz, EasyVectorizationLocal, GS] = {
        new ClusteringChainingLocal(
            data,
            chainingID,
            EasyVectorizationLocal[O, V](0)
        )
    }
    /**
     *
     */
    def apply[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingLocal[ID, O, V, Cz, EasyVectorizationLocal, GS] = {
        apply(data, scala.util.Random.nextInt)
    }
}