package Quantum

import breeze.linalg.{DenseMatrix, DenseVector}

object breeze_test extends App {
  val points = DenseVector(1.0, 2.0, 3.0)
  val w = DenseMatrix(Seq.fill(4)(points): _*)
  println(w)

}
