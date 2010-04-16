package gleam.compiler

import gleam.compiler.typing._
import gleam.compiler.node._

import scala.collection.JavaConversions._

class MethodResolver(
    typeResolver : TypeResolver,
    typeNameResolver : TypeNameResolver) {
  
  def getMatchingMethod(clazz : Class[_], node : AControllerMethodExpr, additionalTypes : Map[String, Type] = Map()) = {
    // Get the argument types
    val argTypes = node.getExpr.map { expr =>
      typeResolver.getType(expr, typeNameResolver, additionalTypes).asInstanceOf[SimpleType].getClazz
    }
    
    try {
      Some(clazz.getMethod(node.getIdentifier.getText, argTypes:_*))
    } catch {
      case _ : NoSuchMethodException => None
    }
  }
}