package tree

import tree.core.Strategy
import tree.helper._
import tree.rules._
import java.io.{File, PrintWriter}

import scala.collection.mutable
import scala.collection.mutable.{Map, Stack}

object Dot {

  def main(args: Array[String]): Unit = {
    println("Generating .dot files...")

    println("Generating graphs for (3 + 4) * 5   =>   (5 * 3) + (5 * 4)")

    // TODO: If time permits, refactor this code into function that takes start, end, rules, and depth
    {
      var i = 0
      val writer = new PrintWriter(new File("CommutDis" + i.toString +".dot" ))
      writer.write("digraph CommutDis" + i.toString + " {\n")
      i = i + 1

      val start: tree.Tree = Node(Node(Node(EmptyNode, "3", EmptyNode), "+", Node(EmptyNode, "4", EmptyNode)), "*", Node(EmptyNode, "5", EmptyNode))

      var alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      var letters = Stack[Char]()
      alpha.foreach(letters.push(_))
      writeExprToDot(writer, start, letters, Map())

      writer.write("}")

      writer.close()


      val end = Node(Node(Node(EmptyNode, "5", EmptyNode),"*",Node(EmptyNode, "3", EmptyNode)),"+",Node(Node(EmptyNode, "5", EmptyNode),"*",Node(EmptyNode, "4", EmptyNode)))
      val ordering = universalExpressionTransformer(start, end, mutable.Set(Commutativity(), Distributivity()), 4, mutable.Seq())

      var prev = start
      ordering match {
        case (true, ruleskips) => // seq [Rule , Skips]
          for ((rule, skips) <- ruleskips) {
            println("Generating dot graph " + i.toString)
            var interres = applyOnceWithSkip(prev, skips, rule)
            interres match {
              case (_, Success(intermediate)) =>
                // we have out intermediate expression, generate the dot file
                val writer = new PrintWriter(new File("CommutDis" + i.toString + ".dot"))
                writer.write("digraph CommutDis" + i.toString + " {\n")


                var alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                var letters = Stack[Char]()
                alpha.foreach(letters.push(_))
                writeExprToDot(writer, intermediate, letters, Map())

                writer.write("}")
                writer.close()

                prev = intermediate
                println("Dot graph " + i.toString + " finished")
                i = i + 1
              case _ => { println("Something went wrong deep") }
            }
          }
        case _ => { println("Something went wrong")}
      }

    }


    println("Generating graphs for Associativity")

    {
      var i = 0
      val writer = new PrintWriter(new File("Associativity" + i.toString +".dot" ))
      writer.write("digraph Associativity" + i.toString + " {\n")
      i = i + 1

      val start: tree.Tree = Node(Node(Node(EmptyNode, "3", EmptyNode), "+", Node(EmptyNode, "4", EmptyNode)), "+", Node(EmptyNode, "5", EmptyNode))


      var alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      var letters = Stack[Char]()
      alpha.foreach(letters.push(_))
      writeExprToDot(writer, start, letters, Map())

      writer.write("}")

      writer.close()


      val end = Node(Node(EmptyNode, "3", EmptyNode), "+", Node(Node(EmptyNode, "4", EmptyNode), "+", Node(EmptyNode, "5", EmptyNode)))
      val ordering = universalExpressionTransformer(start, end, mutable.Set(LeftAssociativity()), 2, mutable.Seq())

      var prev = start
      ordering match {
        case (true, ruleskips) => // seq [Rule , Skips]
          for ((rule, skips) <- ruleskips) {
            println("Generating dot graph " + i.toString)
            var interres = applyOnceWithSkip(prev, skips, rule)
            interres match {
              case (_, Success(intermediate)) =>
                // we have out intermediate expression, generate the dot file
                val writer = new PrintWriter(new File("Associativity" + i.toString + ".dot"))
                writer.write("digraph Associativity" + i.toString + " {\n")


                var alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                var letters = Stack[Char]()
                alpha.foreach(letters.push(_))
                writeExprToDot(writer, intermediate, letters, Map())

                writer.write("}")
                writer.close()

                prev = intermediate
                println("Dot graph " + i.toString + " finished")
                i = i + 1
              case _ => { println("Something went wrong deep") }
            }
          }
        case _ => { println("Something went wrong")}
      }

    }


    println("Generating simple graphs section 4 - showing example rewrite rule and result of application")

    {
      var i = 0
      val writer = new PrintWriter(new File("Section4Example" + i.toString +".dot" ))
      writer.write("digraph Section4Example" + i.toString + " {\n")
      i = i + 1

      val start: tree.Tree = Node(EmptyNode, "z", Node(EmptyNode, "4", EmptyNode))

      var alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      var letters = Stack[Char]()
      alpha.foreach(letters.push(_))
      writeExprToDot(writer, start, letters, Map())

      writer.write("}")

      writer.close()


      val end = Node(Node(Node(EmptyNode, "x", EmptyNode),"y", EmptyNode), "z", Node(EmptyNode, "4", EmptyNode))
      val ordering = universalExpressionTransformer(start, end, mutable.Set(section4Example()), 4, mutable.Seq())

      var prev = start
      ordering match {
        case (true, ruleskips) => // seq [Rule , Skips]
          for ((rule, skips) <- ruleskips) {
            println("Generating dot graph " + i.toString)
            var interres = applyOnceWithSkip(prev, skips, rule)
            interres match {
              case (_, Success(intermediate)) =>
                // we have out intermediate expression, generate the dot file
                val writer = new PrintWriter(new File("Section4Example" + i.toString + ".dot"))
                writer.write("digraph Section4Example" + i.toString + " {\n")


                var alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                var letters = Stack[Char]()
                alpha.foreach(letters.push(_))
                writeExprToDot(writer, intermediate, letters, Map())

                writer.write("}")
                writer.close()

                prev = intermediate
                println("Dot graph " + i.toString + " finished")
                i = i + 1
              case _ => { println("Something went wrong deep") }
            }
          }
        case _ => { println("Something went wrong")}
      }

    }



    println("Generating graphs for section 5 - applyOnce and applyOnceWithSkip comparison")

    {
      var i = 0
      val writer = new PrintWriter(new File("Section5applyOnce" + i.toString +".dot" ))
      writer.write("digraph Section5applyOnce" + i.toString + " {\n")
      i = i + 1

      val start: tree.Tree = Node(Node(EmptyNode, "2", EmptyNode), "W", Node(EmptyNode, "4", EmptyNode))

      var alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      var letters = Stack[Char]()
      alpha.foreach(letters.push(_))
      writeExprToDot(writer, start, letters, Map())

      writer.write("}")

      writer.close()


      val end = Node(Node(Node(EmptyNode, "x", EmptyNode), "2", Node(EmptyNode, "y", EmptyNode)), "W", Node(EmptyNode, "4", EmptyNode))
      val ordering = universalExpressionTransformer(start, end, mutable.Set(section5Example()), 4, mutable.Seq())

      var prev = start
      ordering match {
        case (true, ruleskips) => // seq [Rule , Skips]
          for ((rule, skips) <- ruleskips) {
            println("Generating dot graph " + i.toString)
            var interres = applyOnceWithSkip(prev, skips, rule)
            interres match {
              case (_, Success(intermediate)) =>
                // we have out intermediate expression, generate the dot file
                val writer = new PrintWriter(new File("Section5applyOnce" + i.toString + ".dot"))
                writer.write("digraph Section5applyOnce" + i.toString + " {\n")


                var alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                var letters = Stack[Char]()
                alpha.foreach(letters.push(_))
                writeExprToDot(writer, intermediate, letters, Map())

                writer.write("}")
                writer.close()

                prev = intermediate
                println("Dot graph " + i.toString + " finished")
                i = i + 1
              case _ => { println("Something went wrong deep") }
            }
          }
        case _ => { println("Something went wrong")}
      }

    }

    println("Generating graphs for section 5 - applyOnce and applyOnceWithSkip comparison")

    {
      var i = 0
      val writer = new PrintWriter(new File("Section5applyOnceWithSkip" + i.toString +".dot" ))
      writer.write("digraph Section5applyOnceWithSkip" + i.toString + " {\n")
      i = i + 1

      val start: tree.Tree = Node(Node(EmptyNode, "2", EmptyNode), "W", Node(EmptyNode, "4", EmptyNode))

      var alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      var letters = Stack[Char]()
      alpha.foreach(letters.push(_))
      writeExprToDot(writer, start, letters, Map())

      writer.write("}")

      writer.close()


      val end = Node(Node(EmptyNode, "2", EmptyNode), "W", Node(Node(EmptyNode, "x", EmptyNode), "4", Node(EmptyNode, "y", EmptyNode)))
      val ordering = universalExpressionTransformer(start, end, mutable.Set(section5Example()), 4, mutable.Seq())

      var prev = start
      ordering match {
        case (true, ruleskips) => // seq [Rule , Skips]
          for ((rule, skips) <- ruleskips) {
            println("Generating dot graph " + i.toString)
            var interres = applyOnceWithSkip(prev, skips, rule)
            interres match {
              case (_, Success(intermediate)) =>
                // we have out intermediate expression, generate the dot file
                val writer = new PrintWriter(new File("Section5applyOnceWithSkip" + i.toString + ".dot"))
                writer.write("digraph Section5applyOnceWithSkip" + i.toString + " {\n")


                var alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                var letters = Stack[Char]()
                alpha.foreach(letters.push(_))
                writeExprToDot(writer, intermediate, letters, Map())

                writer.write("}")
                writer.close()

                prev = intermediate
                println("Dot graph " + i.toString + " finished")
                i = i + 1
              case _ => { println("Something went wrong deep") }
            }
          }
        case _ => { println("Something went wrong")}
      }

    }


    println("Finished")
  }
}