package tree

import tree.core._

object rules {

  case class helloToEmpty() extends Strategy[Tree] {
    def apply(e: Tree): RewriteResult[Tree] = e match {
      case Node(lr, v, rt) if v == "Hello" => Success(EmptyNode) // replace a node with value "hello" with an empty node
      case _ => Failure(helloToEmpty()) // if the rule application fails, tell us what rule it was that failed
    }

    override def toString(): String = s"helloToEmpty"
  }




  // Rule:    x  ->  o
  //                / \
  //               x   x
  case class generateNodeFromEmpty() extends Strategy[Tree] {
    def apply(e: Tree): RewriteResult[Tree] = e match {
      case EmptyNode => Success(Node(EmptyNode, "X", EmptyNode)) // problem? - the value here matters when checking for equality, e.g. when checking that the application of the rule has resulted in the desried goal expression
      case _ => Failure(generateNodeFromEmpty())
    }
  }


//  Rule:     o      ->      o
//           / \            / \
//          x   o          x   x
//             / \
//            x   x
  case class destroyOnlyRightChild() extends Strategy[Tree] {
    def apply(e: Tree): RewriteResult[Tree] = e match {
      case Node(EmptyNode, v, rt) =>
        rt match {
          case Node(EmptyNode, v2, EmptyNode) => Success(Node(EmptyNode, v, EmptyNode))
          case _ => Failure(destroyOnlyRightChild())
        }
      case _ => Failure(destroyOnlyRightChild())
    }

    override def toString(): String = s"destroyOnlyRightChild"
  }

}
