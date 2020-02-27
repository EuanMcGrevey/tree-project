package tree

package object core {

  type Strategy[P] = P => RewriteResult[P]

}

// collection of helper functions
package object helper {

  def rewriteResultToBool(res: RewriteResult[_]) : Boolean = {
    res match {
    case Success(_) => true
    case Failure(_) => false
    }
  }


  def rewriteResultToTree(res: RewriteResult[Tree]) : Tree = {
    res match {
      case Success(x) => x
      case Failure(_) => ???
    }
  }

}