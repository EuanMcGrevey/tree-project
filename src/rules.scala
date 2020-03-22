package tree

import tree.core._

object rules {

  // neutrality x + 0 => x and 0 + x => x
  case class PlusMinusNeutrality() extends Strategy[Tree] {
    def apply(e: Tree): RewriteResult[Tree] = e match {
      case Node(lr, v, rt) if v == "+" || v == "-" =>
        rt match {
          case Node(_, v1, _) if v1 == "0" =>
            Success(lr) // lr + 0 == lr
          case _ =>
            lr match {
              case Node(_, v2, _) if v2 == "0" =>
                Success(rt)
              case _ => Failure(PlusMinusNeutrality())
            }
        }
      case _ => Failure(PlusMinusNeutrality())
    }
    override def toString(): String = s"PlusMinusNeutrality"
  }

  // neutrality x * 1 => x and 1 * x => x
  case class MultiplyNeutrality() extends Strategy[Tree] {
    def apply(e: Tree): RewriteResult[Tree] = e match {
      case Node(lr, v, rt) if v == "*" =>
        rt match {
          case Node(_, v1, _) if v1 == "1" =>
            Success(lr) // lr * 1 == lr
          case _ =>
            lr match {
              case Node(_, v2, _) if v2 == "1" =>
                Success(rt)
              case _ => Failure(MultiplyNeutrality())
            }
        }
      }
    override def toString(): String = s"MultiplyNeutrality"
  }

  case class DivisionNeutrality() extends Strategy[Tree] {
    def apply(e: Tree): RewriteResult[Tree] = e match {
      case Node(lr, v, rt) if v == "/" =>
        rt match {
          case Node(_, v1, _) if v1 == "1" =>
            Success(lr)
          case _ => Failure(DivisionNeutrality())
        }
      case _ => Failure(DivisionNeutrality())
    }
    override def toString(): String = s"DivisionNeutrality"
  }


  // Annihilation
  case class MultiplyAnnihilation() extends Strategy[Tree] {
    def apply(e: Tree): RewriteResult[Tree] = e match {
      case Node(lr, v, rt) if v == "*" =>
        rt match {
          case Node(_, v1, _) if v1 == "0" =>
            Success(rt) // lr * 0 == 0
          case _ =>
            lr match {
              case Node(_, v2, _) if v2 == "0" => Success(lr) // 0 * rt == 0
              case _ => Failure(MultiplyAnnihilation())
            }
        }
      case _ => Failure(MultiplyAnnihilation())
    }
    override def toString(): String = s"MultiplyAnnihilation"
  }

  // Associativity - Order of Operations irrelavent - Order of two or more of "+" or "*" in same precedence doesn't matter
  case class LeftAssociativity() extends Strategy[Tree] {
    def apply(e: Tree): RewriteResult[Tree] = e match {
      case Node(lr, v, c) if v == "+" || v == "*" =>
        lr match {
          case Node(a, v1, b) if v1 == v =>
            Success(Node(a, v, Node(b, v, c))) // (a + b) + c == a + (b + c) -- similarly for *
          case _ => Failure(LeftAssociativity())
        }
      case _ => Failure(LeftAssociativity())
    }
    override def toString(): String = s"LeftAssociativity"
  }

  case class RightAssociativity() extends Strategy[Tree] {
    def apply(e: Tree): RewriteResult[Tree] = e match {
      case Node(a, v, rt) if v == "+" || v == "*" =>
        rt match {
          case Node(b, v1, c) if v1 == v =>
            Success(Node(Node(a, v, b), v, c)) // a + (b + c) == (a + b) + c -- similarly for *
          case _ => Failure(RightAssociativity())
        }
      case _ => Failure(RightAssociativity())
    }
    override def toString(): String = s"RightAssociativity"
  }

  // Commutativity - Order of Operands irrelavent
  case class Commutativity() extends Strategy[Tree] {
    def apply(e: Tree): RewriteResult[Tree] = e match {
      case Node(lr, v, rt) if v == "+" || v == "*" =>
        Success(Node(rt, v, lr)) // sub a + b => b + a
      case _ => Failure(Commutativity())
    }
    override def toString(): String = s"Commutativity"
  }

  // a - b == -(b - a) -- don't know if this is necessary
  case class NegateSubtractionReverseArgs() extends Strategy[Tree] {
    def apply(e: Tree): RewriteResult[Tree] = e match {
      case Node(lr, v, rt) if v == "-" =>
        Success(Node(Node(EmptyNode, "-1", EmptyNode), "*", Node(rt, "-", lr)))
      case _ => Failure(NegateSubtractionReverseArgs())
    }

    override def toString(): String = s"Double Negation Equivalence"
  }

  // Distributivity - Multiplication of real numbers distributes over Addition - a * (b + c) == a * b + a * c
  case class Distributivity() extends Strategy[Tree] {
    def apply(e: Tree): RewriteResult[Tree] = e match {
      case Node(a, v, rt) if v == "*" =>
        rt match {
          case Node(b, v1, c) if v1 == "+" =>
            Success(Node(Node(a, "*", b), "+", Node(a, "*", c))) // a * (b + c) => a*b + a*c
          case Node(b, v1, c) if v1 == "-" =>
            Success(Node(Node(a, "*", b), "-", Node(a, "*", c))) // a * (b - c) => a*b - a*c // a bit redundant as this is a special case of the above rewrite, where c is actually -1 * something
          case _ => Failure(Distributivity())
//           not necessary as just commute the arguments first then apply Distributivity as normal
//                      a match {
//              case Node(d, v2, e) if v2 == "+" =>
//                Success(Node(Node(d, "*", rt, "+", Node(e, "*", rt))
//              case Node(d, v2, e) if v2 == "-" =>
//                Success(Node(Node(d, "*", rt, "-", Node(e, "*", rt))
//              case _ => Failure(Distributivity())
//            }
        }
      case _ => Failure(Distributivity())
    }
    override def toString(): String = s"Distributivity"
  }


  //

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

    override def toString(): String = s"generateNodeFromEmpty"
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
