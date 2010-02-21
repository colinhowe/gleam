package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.compiler.node.Node
import uk.co.colinhowe.glimpse.compiler.node.Token


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