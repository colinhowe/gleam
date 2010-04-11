package uk.co.colinhowe.gleam.compiler

import uk.co.colinhowe.gleam.compiler.node.AMacroStmt
import scala.collection.mutable.{ Map => MMap }

class ResolvedCallsProvider {
  private val calls = MMap[AMacroStmt, ResolvedCall]()
  
  def add(stmt : AMacroStmt, call : ResolvedCall) = {
    calls(stmt) = call
  }
  
  def get(stmt : AMacroStmt) = {
    if (calls.contains(stmt)) {
      calls(stmt)
    } else {
      null
    }
  }
}