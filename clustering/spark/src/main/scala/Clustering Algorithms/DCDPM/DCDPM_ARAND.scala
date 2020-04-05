package org.clustering4ever.clustering.dcdpm

object ARAND {

  def adjustedRandIndex(
    contingencyTables: Array[Array[Array[Int]]],
    nbPart: Int,
    nbGlobalClusters: Int,
    numRealClusters: Int,
    n: Int
  )= {

    def cn2(n: Int): Double = n * (n - 1) / 2

    var globalContingencyTable = Array.fill[Int](nbGlobalClusters, numRealClusters)(0)
        
        for (m <- 0 until nbPart) {
          for (i <- 0 until nbGlobalClusters) {
            for (j <- 0 until numRealClusters) {
              globalContingencyTable(i)(j) += contingencyTables(m)(i)(j)
            }
          }
        }
        
        val a  = Array.fill[Int](nbGlobalClusters)(0)
        val b  = Array.fill[Int](numRealClusters)(0)
        
        for (i <- 0 until nbGlobalClusters) {
          for (j <- 0 until numRealClusters) {
            a(i) += globalContingencyTable(i)(j)
            b(j) += globalContingencyTable(i)(j)
          }
        }
        
        var index = 0D
        var sumCai2 = 0D
        var sumCbj2 = 0D
        
        for (i <- 0 until nbGlobalClusters) {
          sumCai2 += cn2(a(i))
          for (j <- 0 until numRealClusters){
            index += cn2(globalContingencyTable(i)(j))
            if (i == 0)
              sumCbj2 += cn2(b(j))
          }
        }
        
    val expectedIndex = sumCai2 * sumCbj2 / cn2(n)
        
    (index - expectedIndex) / (((sumCai2 + sumCbj2) / 2) - expectedIndex)
    
  }

}
