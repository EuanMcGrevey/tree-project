package tree

import tree.core._
import tree.helper._
import tree.rules._
import scala.collection.mutable.Set
import scala.collection.mutable.Seq


object Main {
  def main(args: Array[String]): Unit = {

    // universalExpressionTransformer

    {
      println("------")
      println("Test universalExpressionTransformer works in simple case using one rule")
      val begin = Node(Node(EmptyNode, "A", EmptyNode), "B", Node(EmptyNode, "C", EmptyNode))
      val goal = Node(Node(Node(EmptyNode, "X", EmptyNode), "A", EmptyNode), "B", Node(EmptyNode, "C", EmptyNode))
      val res = universalExpressionTransformer(begin, goal, Set(generateNodeFromEmpty()), 3, Seq()) // test with single rule so I can make sure function works at least sometimes
      println(s"Beginning expression = ${begin}")
      println(s"Goal expression = ${goal}")
      println(s"Rules used: Only generateNodeFromEmpty")
      println(s"Expected = (true, Seq((generateNodeFromEmpty,0)))")
      println(s"Got = ${res}")
      println("------")
    }


    {
      println("------")
      println("Test universalExpressionTransformer works with only one rule with multiple applications")
      val begin = Node(Node(EmptyNode, "A", EmptyNode), "B", Node(EmptyNode, "C", EmptyNode))
      val goal = Node(Node(EmptyNode, "A", EmptyNode), "B", Node(Node(EmptyNode, "X", EmptyNode), "C", Node(EmptyNode, "X", EmptyNode)))
      val res = universalExpressionTransformer(begin, goal, Set(generateNodeFromEmpty()), 3, Seq()) // test with single rule so I can make sure function works at least sometimes
      println(s"Beginning expression = ${begin}")
      println(s"Goal expression = ${goal}")
      println(s"Rules used: Only generateNodeFromEmpty")
      println(s"Expected = (true, Seq((generateNodeFromEmpty,2), (generateNodeFromEmpty,4)))")
      println(s"Got = ${res}")
      println("------")
    }

    {
      println("------")
      println("Test universalExpressionTransformer works with two rules")
      val begin = Node(Node(EmptyNode, "A", EmptyNode), "B", Node(EmptyNode, "C", EmptyNode))
      val goal = Node(Node(EmptyNode, "A", EmptyNode), "B", Node(Node(EmptyNode, "X", EmptyNode), "C", Node(EmptyNode, "X", EmptyNode)))
      val res = universalExpressionTransformer(begin, goal, Set(generateNodeFromEmpty(), destroyOnlyRightChild()), 3, Seq()) // test with multiple rules
      println(s"Beginning expression = ${begin}")
      println(s"Goal expression = ${goal}")
      println(s"Rules used: Only generateNodeFromEmpty")
      println(s"Expected = (true, Seq((generateNodeFromEmpty,2), (generateNodeFromEmpty,4)))")
      println(s"Got = ${res}")
      println("------")
    }


    // naiveExpressionTransformerReturnOrder

    {
      println("------")
      println("Test naiveExpressionTransformerReturnOrder works in simple case")
      val begin = Node(Node(EmptyNode, "A", EmptyNode), "B", Node(EmptyNode, "C", EmptyNode))
      val goal = Node(Node(EmptyNode, "A", EmptyNode), "B", Node(Node(EmptyNode, "X", EmptyNode), "C", Node(EmptyNode, "X", EmptyNode)))
      val res = naiveExpressionTransformerReturnOrder(begin, goal, Set(generateNodeFromEmpty(),destroyOnlyRightChild()), 3, Seq())
      println(s"Beginning expression = ${begin}")
      println(s"Goal expression = ${goal}")
      println(s"Rules used: generateNodeFromEmpty and destroyOnlyRightChild")
      println(s"Expected = (true, Seq(generateNodeFromEmpty, generateNodeFromEmpty))")
      println(s"Got = ${res}")
      println("-----")
    }


    // Begin        o      Goal        o     Rule 1: x  ->   o    Rule 2:   o       ->    o
    // Expr =      / \     Expr =       \                   / \            / \           / \
    //            o   o                  o                 x   x          x   o         x   x
    //                                                                       / \
    //                                                                      x   x
    {
      println("------")
      println("Test naiveExpressionTransformerReturnOrder fails when rules can be applied but impossible to achive goal")
      val begin = Node(Node(EmptyNode, "A", EmptyNode), "B", Node(EmptyNode, "C", EmptyNode))
      val goal = Node(EmptyNode, "B", Node(EmptyNode, "C", EmptyNode))

      val res = naiveExpressionTransformerReturnOrder(begin, goal, Set(generateNodeFromEmpty(),destroyOnlyRightChild()), 3, Seq())
      println(s"Beginning expression = ${begin}")
      println(s"Goal expression = ${goal}")
      println(s"Rules used: generateNodeFromEmpty and destroyOnlyRightChild")
      println(s"Expected = (false, Seq())")
      println(s"Got = ${res}")
      println("-----")
    }

    {
      println("-----")
      println("Test naiveExpressionTransformerReturnOrder returns false if possible but outside depth")
      val begin = Node(EmptyNode, "A", EmptyNode)
      val goal = Node(EmptyNode, "A", Node(EmptyNode, "X", Node(EmptyNode, "X", Node(EmptyNode, "X", Node(EmptyNode, "X", EmptyNode)))))
      val res = naiveExpressionTransformerReturnOrder(begin, goal, Set(generateNodeFromEmpty(),destroyOnlyRightChild()), 3, Seq())
      println(s"Beginning expression = ${begin}")
      println(s"Goal expression = ${goal}")
      println(s"Rules used: generateNodeFromEmpty and destroyOnlyRightChild")
      println(s"Expected = (false, Seq())")
      println(s"Got = ${res}")
      println("-----")
    }




    // Begin     o        Goal        o             Rule 1:   x ->  o     Rule 2:   o      ->      o
    // Expr:    / \       Expr:      / \                           / \             / \            / \
    //         o   o                o   o                         x   x           x   o          x   x
    //                                 / \                                           / \
    //                                o   o                                         x   x

    // Note that Rule 2 is useless here, it's just a decoy for the function
    {
      println("------")
      println("Test naiveExpressionTransformer works in simple case")
      val begin = Node(Node(EmptyNode, "A", EmptyNode), "B", Node(EmptyNode, "C", EmptyNode))
      val goal = Node(Node(EmptyNode, "A", EmptyNode), "B", Node(Node(EmptyNode, "X", EmptyNode), "C", Node(EmptyNode, "X", EmptyNode)))
      //var rules = Set( generateNodeFromEmpty(), destroyOnlyRightChild() ) // get type mismatch when try to use rules as argument in next line
      val res = naiveExpressionTransformer(begin, goal, Set(generateNodeFromEmpty(),destroyOnlyRightChild()), 3) // should return true.
      println(s"Beginning expression = ${begin}")
      println(s"Goal expression = ${goal}")
      println(s"Rules used: generateNodeFromEmpty and destroyOnlyRightChild")
      println(s"Expected = true")
      println(s"Got = ${res}")
      println("-----")
    }


    // naiveExpressionTransformer


    // Begin        o      Goal        o     Rule 1: x  ->   o    Rule 2:   o       ->    o
    // Expr =      / \     Expr =       \                   / \            / \           / \
    //            o   o                  o                 x   x          x   o         x   x
    //                                                                       / \
    //                                                                      x   x
    {
      println("------")
      println("Test naiveExpressionTransformer fails when rules can be applied but impossible to achive goal")
      val begin = Node(Node(EmptyNode, "A", EmptyNode), "B", Node(EmptyNode, "C", EmptyNode))
      val goal = Node(EmptyNode, "B", Node(EmptyNode, "C", EmptyNode))

      val res = naiveExpressionTransformer(begin, goal, Set(generateNodeFromEmpty(),destroyOnlyRightChild()), 3)
      println(s"Beginning expression = ${begin}")
      println(s"Goal expression = ${goal}")
      println(s"Rules used: generateNodeFromEmpty and destroyOnlyRightChild")
      println(s"Expected = false")
      println(s"Got = ${res}")
      println("-----")
    }

    {
      println("-----")
      println("Test naiveExpressionTransformer returns false if possible but outside depth")
      val begin = Node(EmptyNode, "A", EmptyNode)
      val goal = Node(EmptyNode, "A", Node(EmptyNode, "X", Node(EmptyNode, "X", Node(EmptyNode, "X", Node(EmptyNode, "X", EmptyNode)))))
      val res = naiveExpressionTransformer(begin, goal, Set(generateNodeFromEmpty(),destroyOnlyRightChild()), 3)
      println(s"Beginning expression = ${begin}")
      println(s"Goal expression = ${goal}")
      println(s"Rules used: generateNodeFromEmpty and destroyOnlyRightChild")
      println(s"Expected = false")
      println(s"Got = ${res}")
      println("-----")
    }


    //     Hello        => should result in Set of size three when calling applyOnceNTimes with helloToEmpty
    //    /     \
    // Hello   Hello
    {
      println("------")
      println("Test applyOnceNTimes works as intended on tree of size 3")
      val t: Tree = Node(Node(EmptyNode, "Hello", EmptyNode), "Hello", Node(EmptyNode, "Hello", EmptyNode))
      val set: Set[Tree] = applyOnceNTimes(t, helloToEmpty(), Set())
      println(s"Original Tree = ${t}")
      println("All possible results of a single application of helloToEmpty to tree")
      set.foreach { println }
      println(s"Expected size of set = 3")
      println(s"Actual size of set = ${set.size}")
      println("------")
    }


    // applyOnceWithDepth

    //        o
    //      /   \
    //     o     o
    //     \     /
    //      o   o
    {
      println("------")
      println("Test applyOnceWithDepth works as intended on tree of size 5")
      val t: Tree = Node(Node(EmptyNode, "Hello", Node(EmptyNode, "Hello", EmptyNode)), "Hello", Node(Node(EmptyNode, "Hello", EmptyNode), "Hello", EmptyNode))
      val set: Set[Tree] = applyOnceNTimes(t, helloToEmpty(), Set())
      println(s"Original Tree = ${t}")
      println("All possible results of a single application of helloToEmpty to tree")
      set.foreach { println }
      println(s"Expected size of set = 5")
      println(s"Actual size of set = ${set.size}")
      println("------")
    }


    // myNewRule

    {
      // test myNewRule replaces Node with value "Hello" with EmptyNode
      val t: Tree = Node(EmptyNode, "Hello", EmptyNode)
      val r = helloToEmpty()(t)
      val newTree = r match {
        case Success(newTree) => newTree
        case _ => ???
      } // newTree should be EmptyNode
      println(t)
      println(r)
      println(newTree)
      println(rewriteResultToBool(r))
      println("")

      var x = 5
      x = x - 1
      println(x)
    }


    // applyEverywhere

    {
      println("test applyEverywhere replaces single Node with value 'Hello' with EmptyNode")
      val t: Tree = Node(Node(EmptyNode, "Hello", EmptyNode), "Middle", Node(EmptyNode, "Right", EmptyNode))

      val newTree = applyEverywhere(t, helloToEmpty())
      println(t)
      println(newTree)
      println("")
    }


    // applyOnce

    // test applyOnce works with helloToEmpty on this tree
    //    o     =     Nonsense      =>         Nonsense
    //   / \    =    /       \      =>         /      \
    //  o   o   = Hello     Hello   =>    EmptyNode   Hello
    {
      println("test applyOnce works with helloToEmpty")
      var t: Tree = Node(Node(EmptyNode, "Hello", EmptyNode), "Nonsense", Node(EmptyNode, "Hello", EmptyNode))
      var r =       applyOnce(t, helloToEmpty())
      println(t)
      r match {
        case Success(t) => println(t)
        case Failure(_) => println("Could not apply rule anywhere")
      }
      println("")
    }

    {
      println("test applyOnce works with helloToEmpty")
      var t: Tree = Node(Node(EmptyNode, "NonsenseL", EmptyNode), "Nonsense", Node(EmptyNode, "Hello", EmptyNode))
      var r =       applyOnce(t, helloToEmpty())
      println(t)
      r match {
        case Success(t) => println(t)
        case Failure(_) => println("Could not apply rule anywhere")
      }
      println("")
    }


    // test applyOnce works with helloToEmpty on this tree
    //    o     =     Nonsense      =>         Nonsense
    //   / \    =    /       \      =>         /      \
    //  o   o   = Nonsense   Nonsense   =>
    {
      println("test applyOnce fails safely")
      var t: Tree = Node(Node(EmptyNode, "Nonsensel", EmptyNode), "Nonsense", Node(EmptyNode, "Nonsenser", EmptyNode))
      var r =       applyOnce(t, helloToEmpty())
      println(t)
      r match {
        case Success(t) => println(t)
        case Failure(_) => println("Could not apply rule anywhere")
      }
    }

  }
}
