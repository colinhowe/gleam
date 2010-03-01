package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.node._
import uk.co.colinhowe.glimpse.compiler.typing.GenericType
import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.MultipleDefinitionError

import scala.collection.mutable.{ ListBuffer, Set => MSet }
import scala.collection.JavaConversions._

class MacroDefinitionFinder(
    val lineNumberProvider : LineNumberProvider,
    implicit val typeProvider : TypeProvider,
    val macroProvider : MacroDefinitionProvider,
    implicit val typeNameResolver : TypeNameResolver)
  extends DepthFirstAdapter with Conversions {
  
  val errors = ListBuffer[CompilationError]()
  val genericsInScope = scala.collection.mutable.Map[String, Type]()

  override def outAMacroDefn(node : AMacroDefn) = {
    val defn : MacroDefinition = node
    macroProvider ! defn

    // Clear any generics in scope
    genericsInScope.clear();
  }
  
  
  override def inAGenericDefn(node : AGenericDefn) {
    val nodeType = typeProvider.getType(node, typeNameResolver)

    // Put this generic in scope
    genericsInScope.put(node.getIdentifier().getText(), nodeType)
  }
}
