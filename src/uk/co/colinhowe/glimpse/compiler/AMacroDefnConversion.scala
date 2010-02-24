package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.node._
import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.compiler.typing.GenericType

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
      args + (arg.getIdentifier().getText() -> ArgumentDefinition(
          arg.getIdentifier().getText(), 
          typeProvider.getType(arg.getType(), typeNameResolver, generics.toMap),
          cascade, 
          arg.getDefault != null))
    })
    
    val restrictions = if (node.getRestriction != null) {
      node.getRestriction.asInstanceOf[ARestriction].getIdentifier.foldLeft(Set[Restriction]())({
        (set, identifier) => set + NameRestriction(identifier.getText)})
    } else {
      Set[Restriction]()
    }

    new MacroDefinition(
        macroName, typeProvider.getType(node.getContentType, typeNameResolver), node.getDynamic != null, restrictions, arguments)
  }
}