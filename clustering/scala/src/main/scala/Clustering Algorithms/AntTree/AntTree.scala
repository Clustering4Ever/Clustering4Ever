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
      return -1
    }
    lazy val TDissim = tree.dissimilar_value(xpos)
    lazy val xplus = tree.most_similar_node(xi, xpos)

    if (tree.ants(xpos).first_time){
      if (Similarity.cosine_similarity(tree.ants(xi).features, tree.ants(xplus).features) < TDissim){
        tree.disconnect(xplus)
        tree.connect(xi, xpos)
        tree.ants(xpos).first_time = false
        -1
      } else {
        tree.ants(xpos).first_time = false
        xplus
      }
    } else {
      if (Similarity.cosine_similarity(tree.ants(xi).features, tree.ants(xplus).features) < TDissim){
        tree.connect(xi, xpos)
        -1
      } else {
        xplus
      }
    }
  }

  /** Classify a tree.
    *
    * @param tree The tree to be classified.
    */
  def classify(tree: Tree): Unit = {
    while (tree.not_connected_ants.nonEmpty){
      val xi = tree.not_connected_ants.dequeue
      var xpos = 0
      while (xpos >= 0) xpos = algorithm(xi, xpos, tree)
    }
  }

  def main(args: Array[String]): Unit = {
    val tree = new Tree("C:\\Users\\Waris\\Desktop\\untitled\\src\\main\\scala\\iris.csv")
    classify(tree)

    val branch = tree.branch.clone()
    println(branch)
  }
}
