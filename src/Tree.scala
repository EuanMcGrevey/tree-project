package tree

sealed trait Tree {

}

case class Node(var left: Tree, var value: String, var right: Tree) extends Tree {

}

case object EmptyNode extends Tree {

}