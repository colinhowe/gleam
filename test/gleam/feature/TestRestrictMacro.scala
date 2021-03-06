package gleam.feature

import org.junit.Test
import org.junit.Assert._

import gleam.compiler.typing.Type
import gleam.MacroNotFoundError
import gleam.compiler.TypeCheckerTest
import gleam.compiler.MacroDefinition
import gleam.compiler.typing.SimpleType
import gleam.compiler.Restriction

class TestRestrictMacro extends TypeCheckerTest {
  
  @Test
  def restrictByName = {
    """
    macro p with s : string
        restrict to div {
      var x = 1
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
    node x with string
    macro p with s : string
        restrict to top level {
      x s
    }
    p "fail"
    """ compilesTo <view><x>fail</x></view>
  }
  
  @Test
  def failTopLevelRestriction = {
    """
    macro p with s : string
        restrict to top level {
      var x = 1
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
    node x with string
    macro p with s : string
        restrict to div {
      x s
    }
    macro div with s : string {
      p "inside div"
    }
    div "ignored"
    """ compilesTo <view><x>inside div</x></view>
  }
  
  @Test
  def failRestrictionInMacroDefn = {
    """
    macro p with s : string
        restrict to section {
      var x = 1
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
    node x with string
    macro p with s : string
        restrict to div, top level {
      x s
    }
    macro div with s : string {
      p "inside div"
    }
    
    p "fail"
    """ compilesTo <view><x>fail</x></view>
  }
  
  @Test
  def passNodeRestriction = {
    """
    node list with generator
    node item with string restrict to list
    list {
      item "hi"
    }
    """ compilesTo <view><list><item>hi</item></list></view>
  }
}
