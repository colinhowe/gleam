package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.typing.CompoundType
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.Generator
import uk.co.colinhowe.glimpse.compiler.typing.GenericType
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.compiler.node._
import uk.co.colinhowe.glimpse.compiler.IdentifierConverter._
import scala.collection.JavaConversions._

class TypeProvider {
  
  def getType(node : Node, typeNameResolver : TypeNameResolver, additionalTypes : Map[String, Type] = Map()) : Type = {
    node match {
      case _ : AFalseExpr | 
           _ : ATrueExpr  | 
           _ : ABoolType 
             => new SimpleType(classOf[java.lang.Boolean])

      case _ : AConstantExpr |
           _ : AIntType 
             => new SimpleType(classOf[java.lang.Integer])
      
      case _ : AStringExpr |
           _ : AStringType
             => new SimpleType(classOf[java.lang.String])
      
      case _ : AGeneratorType => new SimpleType(classOf[Generator])
      
      case _ : AGeneratorExpr => new SimpleType(classOf[Generator])
      
      case defn : AGenericDefn => new GenericType(defn.getIdentifier().getText(), classOf[Object])
      case qualified : AQualifiedType => getType(qualified, typeNameResolver, additionalTypes)
      case compound : ACompoundType => getType(compound, typeNameResolver, additionalTypes)
      case _ => 
      null
    }
  }
  
  private def getType(node : ACompoundType, typeNameResolver : TypeNameResolver, additionalTypes : Map[String, Type]) : Type = {
    // TODO Make compound types reference a Type instead of a Class as the parent type
    val parentType = getType(node.getParenttype(), typeNameResolver).asInstanceOf[SimpleType]
    val subTypes = node.getTypes().map(getType(_, typeNameResolver, additionalTypes))
    new CompoundType(parentType.getClazz, subTypes.toList)
  }
  
  
  private def getType(node : AQualifiedType, typeNameResolver : TypeNameResolver, additionalTypes : Map[String, Type]) : Type = {
    // Check for known overrides
    val typeName = identifierListToString(node.getIdentifier) match {
      case "string" => "java.lang.String"
      case "ref" => "uk.co.colinhowe.glimpse.PropertyReference"
      case s => s
    }
    
    // TODO Check if this is a generic type
    if (additionalTypes.contains(typeName)) {
          println("additional " + node)
      return additionalTypes(typeName)
    } else {
      return typeNameResolver.getClassByName(typeName) match {
        case Some(clazz) => new SimpleType(clazz)
        case None => new SimpleType(this.getClass().getClassLoader().loadClass(typeName))
      }
    }
  }
}