package gleam.compiler

import gleam.compiler.node._
import gleam.compiler.typing.Type
import gleam.compiler.typing.GenericType

import scala.collection.mutable.{ Map => MMap, Buffer }
import scala.collection.JavaConversions._

object AMacroDefnConversion {
  implicit def convert(node : AMacroDefn)(implicit typeProvider : TypeProvider, typeNameResolver : TypeNameResolver) = {
    val macroName = node.getName().getText()
    
    // Add on any generics needed
    val generics = MMap[String, Type]()
    for (pgeneric <- node.getGenericDefn) {
      val generic = pgeneric.asInstanceOf[AGenericDefn]
      generics(generic.getIdentifier.getText) = new GenericType(generic.getIdentifier().getText(), classOf[Object])
    }
        
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
    
    val restrictions = if (node.getRestriction != null) {
      node.getRestriction.asInstanceOf[ARestriction].getIdentifier.foldLeft(Set[Restriction]())({
        (set, identifier) => set + NameRestriction(identifier.getText)})
    } else {
      Set[Restriction]()
    }
    
    val isDynamic = node.getMacroModifier != null && node.getMacroModifier.isInstanceOf[ADynamicMacroModifier]
    val isAbstract = node.getMacroModifier != null && node.getMacroModifier.isInstanceOf[AAbstractMacroModifier]

    val controller = if (node.getController != null) {
      typeProvider.getType(node.getController.asInstanceOf[AController].getType, typeNameResolver, generics.toMap)
    } else {
      null
    }
    
    val contentType = if (node.getWithDefn != null) {
      typeProvider.getType(node.getWithDefn.asInstanceOf[AWithDefn].getContentType, typeNameResolver)
    } else {
      null
    }
    
    new MacroDefinition(
        macroName, contentType, isDynamic, restrictions, arguments, isAbstract, controller)
  }
}