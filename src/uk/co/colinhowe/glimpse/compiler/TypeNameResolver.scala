package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.node._
import scala.collection.JavaConversions._
import uk.co.colinhowe.glimpse.compiler.IdentifierConverter._

class TypeNameResolver(start : Start) {
  
  val imports = findImports(start)
  
  def getClassByName(name : String) : Option[Class[_]] = imports.get(name)
  
  private def findImports(start : Start) : Map[String, Class[_]] = {
    val imports = scala.collection.mutable.Map[String, Class[_]]()
    for (pimport <- start.getPView().asInstanceOf[AView].getImport) {
      val aimport = pimport.asInstanceOf[AImport]
      
      aimport.getImportType() match {
        case single : ASingleImportType => 
          val qualifiedName = identifierListToString(single.getIdentifier)
          val className = single.getIdentifier.last.getText
          imports(className) = this.getClass().getClassLoader().loadClass(qualifiedName)
        case wildcard : AWildcardImportType =>
      }
    }
    
    imports.toMap
  }
}