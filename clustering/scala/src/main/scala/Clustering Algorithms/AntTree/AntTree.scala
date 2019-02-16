import calculations.Similarity
import structures.Tree


/** Object that contains algorithms related to AntTree.
  *
  */
object AntTree {
  /** Moves an ant through the tree.
    *
    * @param xi The Ant.
    * @param xpos The position of the ant.
    * @param tree The tree.
    * @return Return -1 if the ant has been connected and returns its new position otherwise
    */
  def algorithm(xi: Int, xpos: Int, tree: Tree): Int = {
    if (tree.directSuccessors(xpos).size < 2){
      tree.connect(xi, xpos)
      -1
    } else {
      lazy val tDissim = tree.dissimilarValue(xpos)
      lazy val xplus = tree.mostSimilarNode(xi, xpos)

      if (tree.ants(xpos).firstTime) {
        if (Similarity.cosineSimilarity(tree.ants(xi).features, tree.ants(xplus).features) < tDissim) {
          tree.disconnect(xplus)
          tree.connect(xi, xpos)
          tree.ants(xpos).firstTime = false
          -1
        } else {
          tree.ants(xpos).firstTime = false
          xplus
        }
      } else {
        if (Similarity.cosineSimilarity(tree.ants(xi).features, tree.ants(xplus).features) < tDissim) {
          tree.connect(xi, xpos)
          -1
        } else {
          xplus
        }
      }
    }
  }

  @annotation.tailrec
  private def place(xi: Int, xpos: Int, tree: Tree): Int = if (xpos >= 0) place(xi, algorithm(xi, xpos, tree), tree)
  else xpos

  /** Classify a tree.
    *
    * @param tree The tree to be classified.
    */
  def classify(tree: Tree): Unit = {
    while (tree.notConnectedAnts.nonEmpty) {
      val xi = tree.notConnectedAnts.dequeue
      place(xi, 0, tree)
    }
  }

  def main(args: Array[String]): Unit = {
    val tree = new Tree("C:\\Users\\Waris\\Desktop\\anttree-scala\\src\\main\\scala\\iris.csv")
    classify(tree)

    val branch = tree.branch.clone()
    println(branch)
  }
}
