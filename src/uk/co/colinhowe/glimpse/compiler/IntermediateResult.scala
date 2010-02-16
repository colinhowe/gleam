package uk.co.colinhowe.glimpse.compiler

import uk.co.colinhowe.glimpse.compiler.node.Start
import uk.co.colinhowe.glimpse.CompilationError

class IntermediateResult(
    val ast : Start,
    val viewName : String,
    val sourcename : String) {
  val errors = scala.collection.mutable.Buffer[CompilationError]()
}