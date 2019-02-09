package calculations

import math._

/** Similarity calculations.
  *
  */
object Similarity {
    private def square_rooted(x: Vector[Double]): Double = {
      val a = sqrt(x.map(nbr => nbr * nbr).sum)
      (math rint a * 1000) / 1000
    }

  /** Calcul a cosine similarity between to vector
    *
    * @param x A vector.
    * @param y A vector.
    * @return Cosine similarity.
    */
    def cosine_similarity(x: Vector[Double], y: Vector[Double]): Double = {
      val numerator = (for ((a, b) <- x zip y) yield a * b).sum
      val denominator = square_rooted(x) * square_rooted(y)
      (math rint { numerator / denominator } * 1000) / 1000
    }
}
