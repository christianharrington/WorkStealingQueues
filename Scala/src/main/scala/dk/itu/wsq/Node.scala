package dk.itu.wsq

sealed abstract class Tree[N <: Node]
case class Root[N <: Node]() extends Tree[N]
case class LeftTree[N <: Node](parent: N)  extends Tree[N]
case class RightTree[N <: Node](parent: N) extends Tree[N]

trait Node {
}
