package uk.co.colinhowe.glimpse.infrastructure

import uk.co.colinhowe.glimpse.IdentifierNotFoundException
import scala.reflect.BeanProperty
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Assert._

class TestScope extends AssertionsForJUnit {

  @Test
  def getFromNormalScope() {   
    val macroScope = new Scope(null, "view")
    macroScope.add("x", new Integer(1))

    assert(1 === macroScope.get("x"))
  }

  @Test
  def getVariableThatDoesNotExist() {   
    val macroScope = new Scope(null, "view")

    intercept[IdentifierNotFoundException] {
      assert(1 === macroScope.get("y"))
    }
  }

  @Test
  def fallThroughScope() {   
    val parentScope = new Scope(null, "view")
    parentScope.add("x", new Integer(1))

    val childScope = new Scope(parentScope, "view")
    assert(1 === childScope.get("x"))
  }

  @Test
  def fallThroughStoppedByEndOfMacro() {   
    val parentScope = new Scope(null, "view")
    parentScope.add("x", new Integer(1))

    val macroScope = new Scope(parentScope, "macro")
    intercept[IdentifierNotFoundException] {
      assert(1 === macroScope.get("x"))
    }
  }

  @Test
  def replaceVariable() {   
    val scope = new Scope(null, "owner")

    scope.add("x", new Integer(1))
    assert(1 === scope.get("x"))

    scope.replace("x", new Integer(2))
    assert(2 === scope.get("x"))
  }

  @Test
  def replaceDoesNotFallThrough() {
    val parentScope = new Scope(null, "view")
    parentScope.add("x", new Integer(1))

    val macroScope = new Scope(parentScope, "owner")
    macroScope.add("x", new Integer(2))

    assert(2 === macroScope.get("x"))
    assert(1 === parentScope.get("x"))
  }

  /**
   * Simulates the following:
   * macro x with generator g {
   *   div {
   *     include g // Should use the g above and not have scope problems
   *   }
   * }
   */
  @Test
  def sameOwner {   
    val parentScope = new Scope(null, "owner")
    parentScope.add("g", new Integer(1))
    val middleScope = new Scope(parentScope, "macro")
    val childScope = new Scope(middleScope, "owner")

    assert(1 === childScope.get("g"))
  }

  
  @Test
  def differentOwner {   
    val parentScope = new Scope(null, "view")
    parentScope.add("g", new Integer(1))
    val middleScope = new Scope(parentScope, "view")
    val childScope = new Scope(middleScope, "owner")

    intercept[IdentifierNotFoundException] {
      assert(1 === childScope.get("g"))
    }
  }
}

