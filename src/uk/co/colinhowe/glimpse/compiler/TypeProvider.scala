package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.typing.CompoundType
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.Generator
import uk.co.colinhowe.glimpse.compiler.typing.GenericType
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.compiler.node._
import scala.collection.JavaConversions._

class TypeProvider {
  
  def getType(node : Node, additionalTypes : Map[String, Type] = Map()) : Type = {
    // TODO Remove this once we have more written in Scala
    println(node + " 1addtional types: " + additionalTypes)
    val additionalTypesMap = if (additionalTypes == null) Map[String, Type]() else additionalTypes
    
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
      
      case defn : AGenericDefn => new GenericType(defn.getIdentifier().getText(), classOf[Object]);
      case qualified : AQualifiedType => getType(qualified, additionalTypesMap)
      case compound : ACompoundType => getType(compound, additionalTypesMap)
      case _ => null
    }
  }
  
  private def getType(node : ACompoundType, additionalTypes : Map[String, Type]) : Type = {
    // TODO Make compound types reference a Type instead of a Class as the parent type
    val parentType = getType(node.getParenttype()).asInstanceOf[SimpleType]
    val subTypes = node.getTypes().map(getType(_, additionalTypes))
    new CompoundType(parentType.getClazz, subTypes)
  }
  
  
  private def getType(node : AQualifiedType, additionalTypes : Map[String, Type]) : Type = {
    // Determine the full type
    var typeNode = node.getQualifiedType()
    var typeName = ""
    
    println("addtional types: " + additionalTypes)
      
    while (typeNode != null) {
      typeNode match {
        case simple : ASimpleQualifiedType =>
          typeName = simple.getIdentifier().getText() + typeName
          typeNode = null
        case compound : ACompoundQualifiedType =>
          typeName = "." + compound.getIdentifier().getText() + typeName
          typeNode = compound.getQualifiedType()
      }
    }
    
    // Overrides for known types
    // TODO Imports
    typeName = typeName match {
      case "string" => "java.lang.string"
      case "ref" => "uk.co.colinhowe.glimpse.PropertyReference"
      case _ => typeName
    }
    
    // TODO Check if this is a generic type
    if (additionalTypes.contains(typeName)) {
      return additionalTypes(typeName)
    } else {
      val loader = this.getClass().getClassLoader()
      val clazz = loader.loadClass(typeName)
      return new SimpleType(clazz)
    }
  }
}