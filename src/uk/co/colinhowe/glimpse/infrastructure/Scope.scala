package uk.co.colinhowe.glimpse.infrastructure
import uk.co.colinhowe.glimpse.IdentifierNotFoundException

import scala.collection.mutable.HashMap


/*
int y = 1
macro thing with generator(int v1) g {
  include g(v1: 1)
  y++ // Will not compile - outside scope
}

// Can't go into/out of a macro scope
// Track whether each scope is a macro one. If we start a resolution inside a macro scope then we allow fallthrough until the macro scope
// If we start outside a macro scope then we allow fallthrough but skip macro scopes

div {
  int x = 4;
  div {
    p x
    thingA { int v1, int v2 =>
      p v1
      thing { int v1 =>
        p v1 // Closest one
        p v2 // Outside one
        // Altering v2 in here should alter v2 inside thingA but not persist outside
      }
    } 
  }
}


There is a scope but two _types_ of variables inside


At the start of each generator we push things onto a generator-wide scope (e.g. arguments)
When a variable is modified the top-most scope (with the variable) is affected
*/

/**
 * Variable class encapsulates the value of a variable. This allows for the Scope 
 * class to provide a clean way to retrieve a variable for either reading or writing. * TODO Kill this.
 */
private class Variable(var value : Object)

class Scope(val parentScope : Scope, val isMacroScope : Boolean) {

  private val variables = new java.util.HashMap[String, Variable]()
  
  /**
   * Adds the given variable to the current scope.
   * 
   * @param variableName
   * @param value
   */
  def add(variableName : String, value : Object) : Unit = {
    variables.put(variableName, new Variable(value))
  }
  
  
  /**
   * Replaces the given variable in the current scope. If the variable doesn't
   * already exist an exception will be thrown.
   * 
   * @param variableName
   * @param value
   */
  def replace(variableName : String, value : Object) : Unit = {
    get(variableName, isMacroScope).value = value
  }
  
  
  /**
   * Get the given variable from the current scope. If the variable doesn't already
   * exist an exception will be thrown.
   */
  def get(variableName : String) : Object = get(variableName, isMacroScope).value
  
  
  /**
   * Get the given variable from the current scope. If the variable doesn't already
   * exist an exception will be thrown.
   * 
   * This version handles determining whether to fall through based on whether the
   * first scope was a macro scope or not.
   */
  private def get(variableName : String, firstScopeWasMacro : Boolean) : Variable = {
    // Skip this scope if it is a macro scope and the first scope was not macro scope
    if (!firstScopeWasMacro && isMacroScope) {
      if (parentScope != null) {
        return parentScope.get(variableName, firstScopeWasMacro)
      } else {
        throw new IdentifierNotFoundException(variableName)
      }
    }
    
    // If the first scope was macro scope and we've left macro scope then we must abort
    if (firstScopeWasMacro && !isMacroScope) {
      throw new IdentifierNotFoundException(variableName)
    }

    if (!variables.containsKey(variableName)) {
      if (parentScope != null) {
        return parentScope.get(variableName, firstScopeWasMacro)
      } else {
        throw new IdentifierNotFoundException(variableName)
      }
    }
    return variables.get(variableName)
  }
}