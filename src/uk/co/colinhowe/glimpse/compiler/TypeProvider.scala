package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.typing.CompoundType
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter
import uk.co.colinhowe.glimpse.Generator
import uk.co.colinhowe.glimpse.compiler.typing.GenericType
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.compiler.node._
import scala.collection.JavaConversions._

class TypeProvider extends DepthFirstAdapter {
  val types = scala.collection.mutable.Map[Node, Type]()
  val genericsInScope = scala.collection.mutable.Map[String, Type]()

  def get(node : Node) : Type = types(node)
  
  def getType(node : Node) : Type = {
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
      
      case defn : AGenericDefn => null // new GenericType(defn.getIdentifier().getText(), classOf[Object]);
      case qualified : AQualifiedType => getType(qualified)
      case defn : AWithInitVarDefn => getType(defn.getType)
      case compound : ACompoundType => getType(compound)
      case _ => null
    }
  }
  
  def getType(node : ACompoundType) : Type = {
    // TODO Make compound types reference a Type instead of a Class as the parent type
    val parentType = getType(node.getParenttype()).asInstanceOf[SimpleType]
    val subTypes = node.getTypes().map(getType(_))
    new CompoundType(parentType.getClazz, subTypes)
  }
  
  
  def getType(node : AQualifiedType) : Type = {
    // Determine the full type
    var typeNode = node.getQualifiedType()
    var typeName = ""
    
    while (typeNode != null) {
      typeNode match {
        case simple : ASimpleQualifiedType =>
          typeName += simple.getIdentifier().getText()
          typeNode = null
        case compound : ACompoundQualifiedType =>
          typeName += compound.getIdentifier().getText() + "."
          typeNode = compound.getQualifiedType()
      }
    }
    
    // Overrides for known types
    // TODO Imports
    if (typeName.equals("string")) {
      typeName = "java.lang.string"
    }
    
    // TODO Check if this is a generic type
    if (genericsInScope.contains(typeName)) {
      return genericsInScope(typeName)
    } else {
      val loader = this.getClass().getClassLoader()
      val clazz = loader.loadClass(typeName)
      return new SimpleType(clazz)
    }
  }
  
  override def defaultOut(node : Node) {
    val nodeType = getType(node)
    if (nodeType != null) {
      types.put(node, nodeType)
    }
  }
  
  override def inAGenericDefn(node : AGenericDefn) {
    val nodeType = new GenericType(node.getIdentifier().getText(), classOf[Object])//getType(node)

    // TODO This needs to be properly scoped
    // Put this generic in scope
    println("Generic in scope [" + node.getIdentifier().getText() + "]")
    genericsInScope.put(node.getIdentifier().getText(), nodeType)
    types.put(node, nodeType);
  }
  
  override def outAMacroDefn(node : AMacroDefn) {
    // Done with the macro - wipe the generics in scope
    println("Clearing generics in scope")
    genericsInScope.clear();
  }
}