package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.node._
import scala.collection.JavaConversions._

class TypeNameResolver(start : Start) {
  
  val imports = findImports(start)
  
  def getClassByName(name : String) : Option[Class[_]] = imports.get(name)
  
  private def findImports(start : Start) : Map[String, Class[_]] = {
    val imports = scala.collection.mutable.Map[String, Class[_]]()
    for (pimport <- start.getPView().asInstanceOf[AView].getImport) {
      val aimport = pimport.asInstanceOf[AImport]
        
      val qualifiedName = getQualifiedName(aimport.getName())
      val clazzName = getClazzName(aimport.getName())
  
      imports(clazzName) = this.getClass().getClassLoader().loadClass(qualifiedName)
    }
    
    imports.toMap
  }
  
  private def getClazzName(node : PName) : String = {
    node match {
      case node : AQualifiedName => getClazzName(node.getName)
      case node : ASimpleName => node.getIdentifier.getText
    }
  }
  
  private def getQualifiedName(node : PName) : String = {
    node.toString().trim().replaceAll(" ", ".")
  }
}