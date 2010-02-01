package uk.co.colinhowe.glimpse.infrastructure
import uk.co.colinhowe.glimpse.IdentifierNotFoundException

import scala.reflect.BeanProperty

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Assert._

class TestScope extends AssertionsForJUnit {

  @Test
  def getFromNormalScope() {   
    val macroScope = new Scope(null, false)
    macroScope.add("x", new Integer(1))
    
    assert(1 === macroScope.get("x"))
  }

  @Test
  def getVariableThatDoesNotExist() {   
    val macroScope = new Scope(null, false)
    
    intercept[IdentifierNotFoundException] {
      assert(1 === macroScope.get("y"))
    }
  }
  
  @Test
  def fallThroughScope() {   
    val parentScope = new Scope(null, false)
    parentScope.add("x", new Integer(1))
    
    val childScope = new Scope(parentScope, false)
    assert(1 === childScope.get("x"))
  }
  
  @Test
  def fallThroughStoppedByEndOfMacro() {   
    val parentScope = new Scope(null, false)
    parentScope.add("x", new Integer(1))
    
    val macroScope = new Scope(parentScope, true)
    intercept[IdentifierNotFoundException] {
      assert(1 === macroScope.get("x"))
    }
  }

  @Test
  def replaceVariable() {   
    val scope = new Scope(null, false)

    scope.add("x", new Integer(1))
    assert(1 === scope.get("x"))

    scope.replace("x", new Integer(2))
    assert(2 === scope.get("x"))
  }

  @Test
  def replaceDoesNotFallTrhough() {   
    val parentScope = new Scope(null, false)
    parentScope.add("x", new Integer(1))
    
    val macroScope = new Scope(parentScope, true)
    macroScope.add("x", new Integer(2))
    
    assert(2 === macroScope.get("x"))
    assert(1 === parentScope.get("x"))
  }
}
