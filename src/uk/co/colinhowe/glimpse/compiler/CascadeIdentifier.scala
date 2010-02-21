package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import uk.co.colinhowe.glimpse.compiler.node._
import uk.co.colinhowe.glimpse.compiler.typing.Type
import scala.collection.JavaConversions._

object CascadeIdentifier {
  
  def identify(
      node : Node, 
      typeResolver : TypeResolver, 
      nameResolver : TypeNameResolver,
      resolvedCallsProvider : ResolvedCallsProvider
      ) : Map[String, Type] = {
    node match {
      case defn : AMacroDefn =>
        defn.getArgDefn.foldLeft(Map[String, Type]())((cascades, parg) => {
          val arg = parg.asInstanceOf[AArgDefn]
          if (arg.getModifier.exists(_.isInstanceOf[ACascadeModifier])) {
            cascades + (arg.getIdentifier.getText -> 
              typeResolver.getType(arg.getType, nameResolver)
            )
          } else {
            cascades
          }
        })
        
      case stmt : AMacroStmt =>
        // Find the macro actually invoked
        val defn = resolvedCallsProvider.get(stmt)
        if (defn != null) {
          identify(node.parent, typeResolver, nameResolver, resolvedCallsProvider) ++ defn.arguments.foldLeft(Map[String, Type]())((cascades, argTuple) => {
            val arg = argTuple._2
            if (arg.cascade) {
              cascades + (arg.name -> arg.argType)
            } else {
              cascades
            }
          })
        } else {
          identify(node.parent, typeResolver, nameResolver, resolvedCallsProvider)
        }
      case null =>
        Map[String, Type]()
      case _ =>
        identify(node.parent, typeResolver, nameResolver, resolvedCallsProvider)
    }      
  }
}

class CascadeIdentifier(
    typeResolver : TypeResolver,
    nameResolver : TypeNameResolver,
    resolvedCallsProvider : ResolvedCallsProvider) {
  def identify(node : Node) : Map[String, Type] = 
    CascadeIdentifier.identify(node, typeResolver, nameResolver, resolvedCallsProvider)
}