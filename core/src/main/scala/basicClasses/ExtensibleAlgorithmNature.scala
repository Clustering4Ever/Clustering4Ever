package org.clustering4ever.extensibleAlgorithmNature
/**
 *
 */
trait ClusteringAlgorithmNature extends Serializable
/**
 *
 */
case object JenksNaturalBreaks extends ClusteringAlgorithmNature
/**
 *
 */
case object TensorBiclustering extends ClusteringAlgorithmNature
/**
 *
 */
case object KCenters extends ClusteringAlgorithmNature
/**
 *
 */
case object KMeans extends ClusteringAlgorithmNature
/**
 *
 */
case object KModes extends ClusteringAlgorithmNature
/**
 *
 */
case object KPrototypes extends ClusteringAlgorithmNature
/**
 *
 */
case object GaussianMixtures extends ClusteringAlgorithmNature
/**
 *
 */
case object RLA extends ClusteringAlgorithmNature
/**
 *
 */
case object RLAScalar extends ClusteringAlgorithmNature
/**
 *
 */
case object RLABinary extends ClusteringAlgorithmNature
/**
 *
 */
case object RLAMixt extends ClusteringAlgorithmNature
/**
 *
 */
case object EpsilonProximity extends ClusteringAlgorithmNature
/**
 *
 */
case object EpsilonProximityScalar extends ClusteringAlgorithmNature
/**
 *
 */
case object EpsilonProximityBinary extends ClusteringAlgorithmNature
/**
 *
 */
case object EpsilonProximityMixt extends ClusteringAlgorithmNature
/**
 *
 */
trait PreprocessingAlgorithmNature extends Serializable
/**
 *
 */
case object GradientAscent extends PreprocessingAlgorithmNature