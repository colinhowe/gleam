package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.compiler.node.APropertyExpr
import uk.co.colinhowe.glimpse.compiler.node.ASimpleName
import uk.co.colinhowe.glimpse.compiler.node.Node
import uk.co.colinhowe.glimpse.compiler.typing.Type

class TypeResolver(
    val typeProvider : TypeProvider, 
    val macroProvider : MacroDefinitionProvider
  ) extends DepthFirstAdapter {

  private val types = scala.collection.mutable.Map[Node, Type]()
  
  def addType(node : Node, t : Type) {
    types.put(node, t)
  }
  
  def getType(node : Node, additionalTypes : Map[String, Type]) = {
    if (types.contains(node)) {
      types(node)
    } else {
      typeProvider.getType(node, additionalTypes)
    }
  }
  
  
  override def outAPropertyExpr(node : APropertyExpr) {
    if (node.getName().isInstanceOf[ASimpleName]) {
      val simpleName = node.getName().asInstanceOf[ASimpleName]
      
      val name = simpleName.getIdentifier().getText()
      
      if (macroProvider.get(name).size() > 0) {
        this.types.put(node, macroProvider.get(name).iterator.next())
      } else {
        // TODO Get the type out
      }
    } else {
      throw new RuntimeException("Unsupported type of node")
    }
  }
}