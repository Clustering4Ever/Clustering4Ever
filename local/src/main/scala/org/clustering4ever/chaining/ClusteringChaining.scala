package org.clustering4ever.chaining

/**
 * @author Beck Gaël
 */
import org.clustering4ever.clusteringtraits.{ClusteringAlgorithmLocal, ClusteringChaining, ClusteringInformationsLocal, ClusteringModelLocal}
import org.clustering4ever.roottraits.VectorizationIDTypes._
import org.clustering4ever.roottraits._
import shapeless.HMap
import scala.collection.{GenSeq, immutable, mutable}
import scala.language.higherKinds
import scala.reflect.ClassTag
/**
 * This classe intend to run many algorithms parallely on a local system for medium size datasets
 * @tparam O the raw object from which vectorizations came from
 * @tparam V the nature of the working vector
 * @tparam Cz a clusterizable descendant, EasyClusterizable is the basic advise instance
 * @tparam Vecto the current vectorization which gives the current Vector nature
 * @tparam GS the nature of the collection containing Cz[O, V]
 * @param data the dataset ideally sorted by Cz's IDs, if not it's done automatically
 * @param chainableID the ID of this chainable class
 * @param currentVectorization the current vectorization employed
 * @param clusteringInformations informations about clustering results
 */
case class ClusteringChainingLocal[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Vecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, Vecto],
    GS[X] <: GenSeq[X]
](
    val data: GS[Cz[O, V]],
    val chainableID: Int,
    val currentVectorization: Vecto[O, V],
    val clusteringInformations: HMap[ClusteringInformationsMapping] = HMap.empty[ClusteringInformationsMapping]
)(implicit val ct: ClassTag[Cz[O, V]]) extends ClusteringChaining[O, V, Cz, Vecto[O, V], GS] {
    /**
     * Boolean to know if the dataset is sorted by Cz's ID, it is neccessary depending on some clustering algorithms
     */
    private val sortedDataset = false
    /**
     *
     */
    val dataSortedByID = if(!sortedDataset) {
        val sorted = data.seq.sortBy(_.id)
        val builder = data.genericBuilder[Cz[O, V]].asInstanceOf[mutable.Builder[Cz[O, V], GS[Cz[O, V]]]]
        builder.sizeHint(data.size)
        builder ++= sorted
        builder.result
    } else data
    /**
     *
     */
    private implicit val currentVectorizationMapping = currentVectorization.vectoMapping
    /**
     *
     */
    private implicit val currentClusteringInformationMapping = ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[O, V, Vecto]]
    /**
     * HMap which contains vectorization by vectorizationID
     */
    val vectorizations: HMap[VectorizationMapping] = HMap[VectorizationMapping](currentVectorization.vectorizationID -> currentVectorization)
    /**
     *
     */
    protected[clustering4ever] val fusionChainableSecurity: Int = 0
    /**
     * ClusteringChainingLocal type with a specific GVector and Vectorization
     */
    type Self[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]] = ClusteringChainingLocal[O, GV, Cz, OtherVecto, GS]
    /**
     *
     */
    protected[clustering4ever] def fusionChainable(ccl: Self[V, Vecto]): Self[V, Vecto] = {
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
            dataSortedByID.zip(ccl.dataSortedByID).map{ case (cz1, cz2) => cz1.addClusterIDs(cz2.clusterIDs.takeRight(ccl.fusionChainableSecurity):_*) }.asInstanceOf[GS[Cz[O, V]]],
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
    def runAlgorithm(algorithm: ClusteringAlgorithmLocal[V, _ <: ClusteringModelLocal[V]]): Self[V, Vecto] = {
        val model = algorithm.fit(dataSortedByID)
        val updatedRunNumber = clusteringRunNumber + 1
        val updatedData = model.obtainClustering(dataSortedByID)
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
            private val sortedDataset = true
            override protected[clustering4ever] val fusionChainableSecurity = 1
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
            (new ClusteringChainingLocal(dataSortedByID, chainableID, currentVectorization, clusteringInformations) {
                override val vectorizations = updatedVectorizations
                override val clusteringRunNumber = updatedRunNumber
                private val sortedDataset = true
                override protected[clustering4ever] val fusionChainableSecurity = updatedFusionChainableSecurity
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
    def updateVectorization[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, GV])(implicit ct: ClassTag[Cz[O, GV]]): Self[GV, OtherVecto] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new ClusteringChainingLocal(
            dataSortedByID.map(_.updateVectorization(vectorization)).asInstanceOf[GS[Cz[O, GV]]],
            chainableID,
            vectorization,
            clusteringInformations
        ) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedRunNumber
            private val sortedDataset = true
            override protected[clustering4ever] val fusionChainableSecurity = updatedFusionChainableSecurity
        }
    }
    /**
     * Add a vectorization in ClusteringChaining object which can be applied latter
     */
    def addVectorization[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, GV]): Self[V, Vecto] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new ClusteringChainingLocal(
            dataSortedByID,
            chainableID,
            currentVectorization,
            clusteringInformations
        ) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedRunNumber
            private val sortedDataset = true
            override protected[clustering4ever] val fusionChainableSecurity = updatedFusionChainableSecurity
        }
    }
    /**
     *
     */
    def addVectorizationOnData[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, GV]): Self[V, Vecto] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new ClusteringChainingLocal(
            dataSortedByID.map(_.addVectorization(vectorization)).asInstanceOf[GS[Cz[O, V]]],
            chainableID,
            currentVectorization,
            clusteringInformations
        ) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedRunNumber
            private val sortedDataset = true
            override protected[clustering4ever] val fusionChainableSecurity = updatedFusionChainableSecurity
        }
    }
    /**
     * Update the current working vector for another existing vector in clusterizable vectorized field
     */
    def switchForExistingVectorization[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, GV])(implicit ct: ClassTag[Cz[O, GV]]): Self[GV, OtherVecto] = {
        val updatedVectorData = dataSortedByID.map(_.switchForExistingVectorization(vectorization)).asInstanceOf[GS[Cz[O, GV]]]
        val updatedCurrentVectorization = vectorizations.get(vectorization.vectorizationID)(VectorizationMapping[VectorizationID, OtherVecto[O, GV]]).get
        val updatedGlobalRunNumber = clusteringRunNumber
        val fusionChainableSecurityCopy = fusionChainableSecurity
        val updatedVectorizations = vectorizations
        new ClusteringChainingLocal(updatedVectorData, chainableID, updatedCurrentVectorization, clusteringInformations) {
            override val vectorizations = updatedVectorizations
            override val clusteringRunNumber = updatedGlobalRunNumber
            private val sortedDataset = true
            override protected[clustering4ever] val fusionChainableSecurity = fusionChainableSecurityCopy
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
    def apply[O, V <: GVector[V], Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]], chainingID: Int, isDatasetSortedByID: Boolean)(implicit ct: ClassTag[Cz[O, V]]): ClusteringChainingLocal[O, V, Cz, EasyVectorizationLocal, GS] = {
        new ClusteringChainingLocal(
            data,
            chainingID,
            EasyVectorizationLocal[O, V](0)
        ) {
            private val sortedDataset = isDatasetSortedByID
        }
    }
    /**
     *
     */
    def apply[O, V <: GVector[V], Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]], isDatasetSortedByID: Boolean)(implicit ct: ClassTag[Cz[O, V]]): ClusteringChainingLocal[O, V, Cz, EasyVectorizationLocal, GS] = {
        apply(data, scala.util.Random.nextInt, isDatasetSortedByID)
    }
}