package tree

import tree.core.Strategy

import scala.collection.mutable.{Seq, Set}

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



  // will try to apply at every node, will return EmptyNode if failed, or the the modified tree if succeeded (which may be an EmptyNode)
  def applyEverywhere(t: Tree, /*cond: Tree => Boolean,*/ r: Strategy[Tree]): Tree = {
    //      if (cond(t)) {
    //
    //      } else {
    //          ...
    //      }
    r(t) match {
      case Success(newT) => newT // doesn't this mean it won't try to apply the rule at any other node if it succeeds first try?
      case Failure(_) =>
        t match {
          case EmptyNode => EmptyNode
          case Node(ln, s, rn) =>
            Node(applyEverywhere(ln, r), s, applyEverywhere(rn, r))
        }
    }
  }

  // same as applyEverywhere, except if we succeed in applying the strategy anywhere at all, we stop and return
  // note that this function doesn't care where the rule can be applied, just that it can be applied somewhere
  def applyOnce(t: Tree, r: Strategy[Tree]): RewriteResult[Tree] = {
    r(t) match {
      case Success(newT) => Success(newT)
      case Failure(_) =>
        t match {
          case EmptyNode => Failure(r) // Failure returns
          case Node(ln, s, rn) =>
            // if applying to t doesn't work, try to apply to the left subtree
            applyOnce(ln, r) match {
              // if this works, all good
              case Success(newlT) => Success(Node(newlT, s, rn))
              // otherwise, try the right subtree
              case Failure(_) =>
                applyOnce(rn, r) match {
                  case Success(newrT) => Success(Node(ln, s, newrT))
                  case Failure(_) => Failure[Tree](r : Strategy[Tree]) // couldn't apply rule to any node in tree
                }
            }
        }

    }
  }

  //    def apply(t: Tree, r: Strategy[Tree]): Set[Tree] = {
  //
  //      def recurse() = {
  //        t match {
  //          case EmptyNode => Set()
  //          case Node(ln, s, rn) =>
  //            // if applying to t doesn't work, try to apply to the left subtree
  //            apply(ln, r) match {
  //              // if this works, all good
  //              case set =>
  //                set.foreach {
  //                  newlT => Success(Node(newlT, s, rn))
  //                } +: recurseRight()
  //              // otherwise, try the right subtree
  //              case Failure(_) =>
  //                apply(rn, r) match {
  //                  case Success(newrT) => Success(Node(ln, s, newrT))
  //                  case Failure(_) => Failure[Tree](r : Strategy[Tree]) // couldn't apply rule to any node in tree
  //                }
  //            }
  //        }
  //      }
  //
  //      r(t) match {
  //        case Success(newT) =>
  //          Set(Success(newT)) +: recurse()
  //        case Failure(_) => recurse()
  //
  //      }
  //    }

  // returns the result of one successful application of a given rule on a given tree
  // supplied with a skip number, which is the number of successful rule applications to skip before returning
  def applyOnceWithSkip(t: Tree, skip: Int, r: Strategy[Tree]): (Int, RewriteResult[Tree]) = {

    def recurse(skip: Int): (Int, RewriteResult[Tree]) = {
      t match {
        case EmptyNode => (skip, Failure(r)) // Failure returns
        case Node(ln, s, rn) =>
          // if applying to t doesn't work, try to apply to the left subtree
          applyOnceWithSkip(ln, skip, r) match {
            // if this works, all good
            case (0, Success(newlT)) => (0, Success(Node(newlT, s, rn)))
            case (_, Success(_)) => ??? // TODO Convince self this would never get triggered
            // otherwise, try the right subtree
            case (ss, Failure(_)) =>
              applyOnceWithSkip(rn, ss, r) match {
                case (0, Success(newrT)) => (0, Success(Node(ln, s, newrT)))
                case (_, Success(_)) => ??? // TODO Convince self this would never get triggered
                case (sss, Failure(_)) => (sss, Failure[Tree](r : Strategy[Tree])) // couldn't apply rule to any node in tree
              }
          }
      }
    }

    r(t) match {
      case Success(newT) =>
        if (skip == 0) {
          (0, Success(newT))
        } else {
          recurse(skip - 1)
        }
      case Failure(_) =>
        recurse(skip)
    }
  }



  // returns the set containing all successful applications of a given rule on a given tree
  def applyOnceNTimes(t: Tree, r: Strategy[Tree], set: Set[Tree]): Set[Tree] = {
    applyOnceWithSkip(t, set.size, r) match {
      case (_,Failure(_)) => set // we are done, couldn't find a rule application that resulted in something new
      case (_,Success(newT)) =>
        t match {
          case EmptyNode => set + newT // we succeeded in rule application, but can't recurse anymore
          case Node(lt, v, rt) =>
            applyOnceNTimes(t, r, set + newT) // keep calling this function until applyOnceWithSkip fails.
          // Each success should add an element to the set,
          // and for an n node tree, at most n elements should be added to the set
        }
    }
  }

  def applyOnceNTimesReturnSkip(t: Tree, r: Strategy[Tree], set: Set[(Tree, Int)]): Set[(Tree, Int)] = {
    applyOnceWithSkip(t, set.size, r) match {
      case (_,Failure(_)) => set //
      case (_,Success(newT)) =>
        t match {
          case EmptyNode => set + Tuple2(newT, set.size)
          case Node(lt, v, rt) =>
            applyOnceNTimesReturnSkip(t, r, set + Tuple2(newT, set.size)) // we keep calling this function until we exhaust the tree
        }
    }
  }



  // Two main problems:
  //  - Need some way of avoiding useless computation

  // simple IDEA = Look through the beginning and goal expressions and build up a set of all possible things that
  // the rules that take us from begin to goal could operate on.

  // Then we can look through the set of rules and remove anything that doesn't have at least one of the elements of our above set in it somewhere.

  //  - Need some way of avoiding repeated computation
  //  Keep track of trees we've seen before to avoid repeated computation of the same applyOnceNTimes call with the same tree and rule?

  // Takes and begin and goal expression, and a set of rules.
  // Returns true if can go from the beginning to goal using the rules in the set
  def naiveExpressionTransformer(begin: Tree, goal: Tree, rules: Set[Strategy[Tree]], depth: Int): Boolean = {
    // will go through at most 3 iterations. This is needed to curb the exponential growth of the naive transformer.
    if (depth == 0) return false

    var candidates : Set[Tree] = Set()
    for (rule <- rules) {
      val rulecans = applyOnceNTimes(begin, rule, Set())
      candidates ++= rulecans
    }
    // candidates should now hold all possible expressions we could get from successfully applying one of the rules in the provided set once.

    for (can <- candidates) {
      if (can == goal) return true
    }
    // { -- optional candidate pruning stage here -- } //
    for (can <- candidates) if (naiveExpressionTransformer(can, goal, rules, depth - 1)) return true
    return false // if all else fails
  }


  // next steps:
  // - Have the naive expression transformer return the rules in order of application if it succeeds
  // - Then see if can say where to apply the rules - Either add a unique identifier to each node, or similar to skip, say how many times to skip rule application during pre-order traversal
  // - See if can be smarter about which rules we bother applying. Is there any way to traverse the source code looking for patterns that come up in either the beginning or goal expression?
  //   Would this even be faster?

  // same as NaiveExpressionTransformer but this time return the order of rule application
  // Failure is represented as an empty list

  def stillNaiveExpressionTransformer(begin: Tree, goal: Tree, rules: Set[Strategy[Tree]], depth: Int, appOrder: Seq[Strategy[Tree]]): (Boolean, Seq[Strategy[Tree]]) = {
    if (depth == 0) return (false, Seq()) // couldn't do the tranformation in less than 5 iterations

    // we track which rule led to which resultant trees, so we can return the order
    var candidates : Set[(Strategy[Tree],Set[Tree])] = Set()
    for (rule <- rules) {
      val rulecans = applyOnceNTimes(begin, rule, Set()) // rulecans has type Set[Tree]
      candidates += Tuple2(rule , rulecans)  // that is to say, the trees in the set on the right got there from the rule on the left
    }


    for ((rule, rulecans) <- candidates) {
      for (can <- rulecans) if (can == goal) return (true, appOrder :+ rule)
    }

    // we couldn't find a succesful rule application in this iteration, go deeper
    for ((rule, rulecans) <- candidates) {
      for (can <- rulecans) {
        stillNaiveExpressionTransformer(can, goal, rules, depth-1, appOrder :+ rule) match {
          case (true, newAppOrder) => return (true, newAppOrder)
          case (false, _) => (false, Seq()) // just keep going, next can
          case _ => ??? // panic
        }
      }
    }

    return (false, Seq())
  }


  // same as stillNaiveExpressionTransformer but returns the number of skips in a pre-order traversal needed to recreate the transformation
  def evenStillNaiveExpressionTransformer(begin: Tree, goal: Tree, rules: Set[Strategy[Tree]], depth: Int, appOrder: Seq[Tuple2[Strategy[Tree], Int]]) : (Boolean, Seq[Tuple2[Strategy[Tree], Int]]) = {
    if (depth == 0) return (false, Seq())

    // we track which rule led to which resultant trees, as well as the number of skips for each tree-rule combination
    var candidates : Set[( Strategy[Tree] , Set[(Tree, Int)] )] = Set()
    for (rule <- rules) {
      val rulecans = applyOnceNTimesReturnSkip(begin, rule, Set()) // rulecans has type Set [ (Tree , int) ]
      candidates += Tuple2(rule , rulecans)
    }

    for ((rule, rulecans) <- candidates) {
      for ((can, skips) <- rulecans) {
        if (can == goal) return (true, appOrder :+ (rule, skips))
      }
    }

    for ((rule, rulecans) <- candidates) {
      for ((can, skips) <- rulecans) {
        evenStillNaiveExpressionTransformer(can, goal, rules, depth-1, appOrder :+ (rule, skips)) match {
          case (true, newAppOrder) => return (true, newAppOrder)
          case (false, _) => (false, Seq()) // just keep going
          case _ => ???
        }
      }
    }

    return (false, Seq())
  }

}