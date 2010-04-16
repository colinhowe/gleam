package gleam.compiler.ast

import gleam.compiler.node._
import scala.collection.JavaConversions._

class AstProducer {
  def createAst(start : Start) : GView = {
    def imports = start.getPView.asInstanceOf[AView].getImport.map {
      _.asInstanceOf[AImport].getImportType match {
        case imp : ASingleImportType =>
          GSingleImport(imp.getIdentifier.foldLeft("")(_ + "." + _))
        case imp : AWildcardImportType =>
          GWildcardImport(imp.getIdentifier.foldLeft("")(_ + "." + _) + ".*")
      }
    }

    GView(imports.toList, null, null, null)
  }
}