package org.clustering4ever.preprocessing.umap

import breeze.linalg._

sealed trait UMAPInitialization extends Serializable

case object RandomInit extends UMAPInitialization
case object SpectralInit extends UMAPInitialization
case class CustomInit(val matrix: DenseMatrix[Double]) extends UMAPInitialization