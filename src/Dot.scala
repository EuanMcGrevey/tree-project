package tree

import tree.core._
import tree.helper._
import tree.rules._

import java.io.{File, PrintWriter}
import scala.collection.mutable.Stack

object Dot {

  def main(args: Array[String]): Unit = {
    println("Generating .dot files...")

    {
      val writer = new PrintWriter(new File("test1.dot" ))

      writer.write("digraph test {\n")

      val expr = Node(Node(EmptyNode, "4", EmptyNode), "+", Node(EmptyNode, "3", EmptyNode))

      var alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      var letters = Stack[Char]()
      alpha.foreach(letters.push(_))
      writeExprToDot(writer, expr, letters)

      writer.write("}")

      writer.close()
    }

    {
      val writer = new PrintWriter(new File("3times4plus5.dot" ))

      writer.write("digraph simpleexpression {\n")

      // 3 * ( 4 + 5 )
      val expr = Node(Node(EmptyNode, "3", EmptyNode), "*", Node(Node(EmptyNode, "4", EmptyNode), "+", Node(EmptyNode, "5", EmptyNode)))

      var alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      var letters = Stack[Char]()
      alpha.foreach(letters.push(_))
      writeExprToDot(writer, expr, letters)

      writer.write("}")

      writer.close()
    }

    println("Finished")
  }
}