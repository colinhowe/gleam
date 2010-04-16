package gleam.compiler

import gleam.compiler.analysis.DepthFirstAdapter
import gleam.compiler.node.Node
import gleam.compiler.node.Token


class LineNumberProvider extends DepthFirstAdapter {
  private val firstLineNumbers = scala.collection.mutable.Map[Node, Int]()
  
  override def defaultCase(node : Node) {
    node match {
      case node : Token => {
        var currentParent = node.parent()
        val line = node.getLine()
        while (currentParent != null) {
          if (!firstLineNumbers.contains(currentParent)) {
            firstLineNumbers.put(currentParent, line)
          }
          currentParent = currentParent.parent()
        }
      }
    }
    super.defaultCase(node)
  }
  
  def getLineNumber(node : Node) = firstLineNumbers.get(node)
}