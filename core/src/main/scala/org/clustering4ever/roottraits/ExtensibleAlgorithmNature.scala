package org.clustering4ever.roottraits

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
case object RLAMixed extends ClusteringAlgorithmNature
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
case object EpsilonProximityMixed extends ClusteringAlgorithmNature
/**
 *
 */
case object AntTree extends ClusteringAlgorithmNature
/**
 *
 */
case object AntTreeScalar extends ClusteringAlgorithmNature
/**
 *
 */
case object AntTreeBinary extends ClusteringAlgorithmNature
/**
 *
 */
case object AntTreeMixed extends ClusteringAlgorithmNature
/**
 *
 */
trait PreprocessingAlgorithmNature extends Serializable
/**
 *
 */
case object GradientAscent extends PreprocessingAlgorithmNature
/**
  *
  */
case object Optics extends ClusteringAlgorithmNature
/**
  *
  */
case object OpticsScalar extends ClusteringAlgorithmNature
/**
  *
  */
case object OpticsBinary extends ClusteringAlgorithmNature
/**
  *
  */
case object OpticsMixed extends ClusteringAlgorithmNature