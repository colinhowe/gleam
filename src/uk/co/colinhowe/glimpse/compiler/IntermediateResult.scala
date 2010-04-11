package uk.co.colinhowe.gleam.compiler

import uk.co.colinhowe.gleam.compiler.node.Start
import uk.co.colinhowe.gleam.CompilationError

class IntermediateResult(
    val ast : Start,
    val viewName : String,
    val sourcename : String) {
  val errors = scala.collection.mutable.Buffer[CompilationError]()
  var lineNumberProvider = new LineNumberProvider
  var typeNameResolver : TypeNameResolver = null
}