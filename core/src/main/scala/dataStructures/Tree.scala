package org.clustering4ever.structures.tree
/**
 * @author Beck Gaël
 */
import scala.collection.mutable
/**
 * Basic generic definition of a Tree
 */
sealed trait Tree[+T] {
  val id: Int
}
/**
 * Generic definition of a Leaf
 */
final case class Leaf[T](val id: Int, val value: T) extends Tree[T]
/**
 * Generic definition of a Node
 */
final case class Node[T](val id: Int, var childrens: List[Tree[T]]) extends Tree[T]
/**
 * Some basic methods linked to tree
 */
object Tree {
  /**
   * @return the size of the tree
   */
  final def size[T](t: Tree[T]): Int = {
    @annotation.tailrec
    def go(l: List[Tree[T]], acc: Int): Int = { 
      l match {
        case Nil => acc
        case Leaf(_, v) :: ls => go(ls, acc + 1)
        case Node(_, childrens) :: ls => go(childrens ::: ls, acc + 1)
      }
    }
    go(List(t), 0)
  }
  /**
   * @return ids of traversed nodes using depth traversal
   */
  final def depthTraversal[T](t: Tree[T]) = {
    @annotation.tailrec
    def go(l: List[Tree[T]], ids: mutable.Buffer[Int]): mutable.Buffer[Int] = {
      l match {
        case Nil => ids
        case Leaf(id, v) :: ls => go(ls, ids += id)
        case Node(id, childrens) :: ls => go(childrens ::: ls, ids += id)
      }
    }
    go(List(t), mutable.ArrayBuffer.empty[Int])
  }
}