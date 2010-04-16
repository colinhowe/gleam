package gleam.compiler

import gleam.compiler.node.Start
import gleam.CompilationError

class IntermediateResult(
    val ast : Start,
    val viewName : String,
    val sourcename : String) {
  val errors = scala.collection.mutable.Buffer[CompilationError]()
  var lineNumberProvider = new LineNumberProvider
  var typeNameResolver : TypeNameResolver = null
}