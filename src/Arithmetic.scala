package tree

import tree.core._
import tree.helper._
import tree.rules._

import scala.collection.mutable.{Seq, Set}

object Arithmetic {

  def main(args: Array[String]): Unit = {
    // write some tests and possible outputs for the dissertation

    {
      println("------")
      println("Test (3 + 4) * 5   =>   (3 * 5) + (4 * 5)")
      val begin = Node(Node(Node(EmptyNode, "3", EmptyNode), "+", Node(EmptyNode, "4", EmptyNode)), "*", Node(EmptyNode, "5", EmptyNode))
      val goal = Node(Node(Node(EmptyNode, "3", EmptyNode), "*", Node(EmptyNode, "5", EmptyNode)), "+", Node(Node(EmptyNode, "4", EmptyNode), "*", Node(EmptyNode, "5", EmptyNode)))
      val res = applyEverywhere(begin, Distributivity())
      println(s"Beginning expression = ${begin}")
      println(s"Goal expression = ${goal}")
      println(s"Rules used: Only Distributivity")
//      println(s"Expected = (true, Seq((generateNodeFromEmpty,0)))")
      println(s"Got = ${res}")
      println("------")
    }

    {
      println("------")
      println("Test 5 * (3 + 4)   =>   (5 * 3) + (5 * 4)")
      val begin = Node(Node(EmptyNode, "5", EmptyNode), "*", Node(Node(EmptyNode, "3", EmptyNode), "+", Node(EmptyNode, "4", EmptyNode)))
      val goal = Node(Node(Node(EmptyNode, "5", EmptyNode), "*", Node(EmptyNode, "3", EmptyNode)), "+", Node(Node(EmptyNode, "5", EmptyNode), "*", Node(EmptyNode, "4", EmptyNode)))
      val res = applyEverywhere(begin, Distributivity())
      println(s"Beginning expression = ${begin}")
      println(s"Goal expression = ${goal}")
      println(s"Rules used: Only Distributivity")
      //      println(s"Expected = (true, Seq((generateNodeFromEmpty,0)))")
      println(s"Got = ${res}")
      println("------")
    }



    // TODO: Implementation does something interesting here to talk about in dissertation
    //       It commutes (3+4) to (4+3) and back again before commuting ((3+4)*5) to (5*(3+4))
    //       And applying Distributivity to get ((5*3)+(5*4))
    //       4 Steps when 2 is optimal
    // TODO: Much Larger / more complex expression and compare length of sequence with optimal amount.
    {
      println("------")
      println("Test (3 + 4) * 5   =>   (5 * 3) + (5 * 4)")
      val begin = Node(Node(Node(EmptyNode, "3", EmptyNode), "+", Node(EmptyNode, "4", EmptyNode)), "*", Node(EmptyNode, "5", EmptyNode))
      val goal = Node(Node(Node(EmptyNode, "5", EmptyNode), "*", Node(EmptyNode, "3", EmptyNode)), "+", Node(Node(EmptyNode, "5", EmptyNode), "*", Node(EmptyNode, "4", EmptyNode)))
      val res = universalExpressionTransformer(begin, goal, Set(Distributivity(), Commutativity()), 5, Seq())
      println(s"Beginning expression = ${begin}")
      println(s"Goal expression = ${goal}")
      println(s"Rules used: Distributivity and Commutativity")
      println(s"Got = ${res}")
      println("------")
    }
  }

}