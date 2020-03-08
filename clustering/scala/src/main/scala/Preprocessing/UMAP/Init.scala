package org.clustering4ever.scala.umap

import breeze.linalg._

sealed trait UMAPInitialization extends Serializable

case object RandomInit extends UMAPInitialization
case object SpectralInit extends UMAPInitialization
case class CustomInit(val matrix: DenseMatrix[Double]) extends UMAPInitialization