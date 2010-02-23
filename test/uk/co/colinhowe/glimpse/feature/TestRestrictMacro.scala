package uk.co.colinhowe.glimpse.feature

import org.junit.Test
import org.junit.Assert._

import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.MacroNotFoundError
import uk.co.colinhowe.glimpse.compiler.TypeCheckerTest
import uk.co.colinhowe.glimpse.compiler.MacroDefinition
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import uk.co.colinhowe.glimpse.compiler.Restriction

class TestRestrictMacro extends TypeCheckerTest {
  
  @Test
  def restrictByName = {
    """
    macro p with s : string
        restrict to div {
      node p s
    }
    p "fail"
    """ failsWith
    MacroNotFoundError(
      line = 6,
      name = "p",
      argumentTypes = Map[String, Type](),
      valueType = SimpleType(classOf[String]),
      definitionsFound = Set[MacroDefinition](
        new MacroDefinition("p", SimpleType(classOf[String]), false, Set[Restriction]())
      )
    )
  }
  
  @Test
  def passTopLevelRestriction = {
    """
    macro p with s : string
        restrict to top level {
      node p s
    }
    p "fail"
    """ compilesTo <view><p>fail</p></view>
  }
  
  @Test
  def failTopLevelRestriction = {
    """
    macro p with s : string
        restrict to top level {
      node p s
    }
    macro div with g : generator {
      include g
    }
    div {
      p "fail"
    }
    """ failsWith
    MacroNotFoundError(
      line = 10,
      name = "p",
      argumentTypes = Map[String, Type](),
      valueType = SimpleType(classOf[String]),
      definitionsFound = Set[MacroDefinition](
        new MacroDefinition("p", SimpleType(classOf[String]), false, Set[Restriction]())
      )
    )
  }
  
  @Test
  def passRestrictionInMacroDefn = {
    """
    macro p with s : string
        restrict to div {
      node p s
    }
    macro div with s : string {
      p "inside div"
    }
    div "ignored"
    """ compilesTo <view><p>inside div</p></view>
  }
  
  @Test
  def failRestrictionInMacroDefn = {
    """
    macro p with s : string
        restrict to section {
      node p s
    }
    macro div with s : string {
      p "inside div"
    }
    div "ignored"
    """ failsWith
    MacroNotFoundError(
      line = 7,
      name = "p",
      argumentTypes = Map[String, Type](),
      valueType = SimpleType(classOf[String]),
      definitionsFound = Set[MacroDefinition](
        new MacroDefinition("p", SimpleType(classOf[String]), false, Set[Restriction]())
      )
    )
  }
  
  @Test
  def passMultipleRestrictions = {
    """
    macro p with s : string
        restrict to div, top level {
      node p s
    }
    macro div with s : string {
      p "inside div"
    }
    
    p "fail"
    """ compilesTo <view><p>fail</p></view>
  }
}
