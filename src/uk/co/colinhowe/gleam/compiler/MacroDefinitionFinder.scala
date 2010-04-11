package uk.co.colinhowe.gleam.compiler

import uk.co.colinhowe.gleam.compiler.node._
import uk.co.colinhowe.gleam.compiler.typing.GenericType
import uk.co.colinhowe.gleam.compiler.typing.Type
import uk.co.colinhowe.gleam.CompilationError
import uk.co.colinhowe.gleam.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.gleam.MultipleDefinitionError

import scala.collection.mutable.{ ListBuffer, Set => MSet, Map => MMap }
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
    macroProvider ! (node, defn)

    // Clear any generics in scope
    genericsInScope.clear();
  }
  
  override def outANodeDefn(node : ANodeDefn) = {
    
    // TODO This is duplicated in macro definition conversion
    val contentType = if (node.getType != null) {
      typeProvider.getType(node.getType, typeNameResolver)
    } else {
      null
    }
    
      // Add on any generics needed
    val generics = MMap[String, Type]()
    for (pgeneric <- node.getGenericDefn) {
      val generic = pgeneric.asInstanceOf[AGenericDefn]
      generics(generic.getIdentifier.getText) = new GenericType(generic.getIdentifier().getText(), classOf[Object])
    }
    
    // TODO This is duplicated in macro definition conversion
    val arguments = node.getArgDefn().foldLeft(Map[String, ArgumentDefinition]())({ (args, parg) =>
      val arg = parg.asInstanceOf[AArgDefn]
      val cascade = arg.getModifier.exists { _.isInstanceOf[ACascadeModifier] }
      val isRuntimeTyped = arg.getModifier.exists { _.isInstanceOf[ARuntimetypedModifier] }
      args + (arg.getIdentifier().getText() -> ArgumentDefinition(
          arg.getIdentifier().getText(), 
          typeProvider.getType(arg.getType(), typeNameResolver, generics.toMap),
          cascade, 
          arg.getDefault != null,
          isRuntimeTyped))
    })
    
    // TODO This is duplicated in macro definition conversion
    val restrictions = if (node.getRestriction != null) {
      node.getRestriction.asInstanceOf[ARestriction].getIdentifier.foldLeft(Set[Restriction]())({
        (set, identifier) => set + NameRestriction(identifier.getText)})
    } else {
      Set[Restriction]()
    }
    
    val defn : MacroDefinition = MacroDefinition(
      name = node.getName.getText,
      valueType = contentType,
      isDynamic = false,
      isNodeDefn = true,
      restrictions = restrictions,
      arguments = arguments
    )

    macroProvider ! (node, defn)

    // Clear any generics in scope
    genericsInScope.clear();
  }
  
  
  override def inAGenericDefn(node : AGenericDefn) {
    val nodeType = typeProvider.getType(node, typeNameResolver)

    // Put this generic in scope
    genericsInScope.put(node.getIdentifier().getText(), nodeType)
  }
}
