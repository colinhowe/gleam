package uk.co.colinhowe.glimpse.infrastructure;

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Assert._

class TestScopes extends AssertionsForJUnit {

  @Test
  def getFromNormalScope() {   
    val macroScope = new Scope(null, false);
    macroScope.add("x", new Integer(1));
    
    assert(1 === macroScope.get("x"));
  }

  @Test
  def getVariableThatDoesNotExist() {   
    val macroScope = new Scope(null, false);
    
    intercept[IllegalArgumentException] {
      assert(1 === macroScope.get("y"));
    }
  };
  
  @Test
  def fallThroughScope() {   
    val parentScope = new Scope(null, false);
    parentScope.add("x", new Integer(1));
    
    val childScope = new Scope(parentScope, false);
    assert(1 === childScope.get("x"));
  };
  
  @Test
  def fallThroughStoppedByEndOfMacro() {   
    val parentScope = new Scope(null, false);
    parentScope.add("x", new Integer(1));
    
    val macroScope = new Scope(parentScope, true);
    intercept[IllegalArgumentException] {
      assert(1 === macroScope.get("x"));
    }
  };
}