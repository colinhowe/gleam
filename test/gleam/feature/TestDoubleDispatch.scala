package gleam.feature

import gleam.compiler.DummyController
import org.junit.Test
import org.junit.Assert._

import gleam.compiler.typing.Type
import gleam.MacroNotFoundError
import gleam.compiler.TypeCheckerTest
import gleam.compiler.MacroDefinition
import gleam.compiler.typing.SimpleType
import gleam.compiler.Restriction

class TestDoubleDispatch extends TypeCheckerTest {
  
  @Test
  def actualClass = {
    """
    controller gleam.compiler.DummyController
      
    abstract macro dd(runtime typed o : java.lang.Object) with s : string
        
    node p(o : string) with string
    macro dd(runtime typed o : string) with s : string {
      p(o: o) "Success"      
    }      
    
    dd(o: c.someString) "ignored"
    """ controller(new DummyController) compilesTo
    <view><p o="someString">Success</p></view>
  }
  
  @Test
  def parentClass = {
    """
    controller gleam.compiler.DummyController
      
    abstract macro dd(runtime typed o : java.lang.Object) with s : string
        
    node p(o : java.lang.Number) with string
    macro dd(runtime typed o : java.lang.Number) with s : string {
      p(o: o) "Success"
    }      
    
    dd(o: c.someBigInteger) "ignored"
    """ controller(new DummyController) compilesTo
    <view><p o="5">Success</p></view>
  }
  
  @Test
  def bestMatch = {
    """
    controller gleam.compiler.DummyController
      
    abstract macro dd(runtime typed o : java.lang.Object) with s : string
        
    node p(o : java.lang.Object) with string
    macro dd(runtime typed o : java.lang.Number) with s : string {
      p(o: o) "Number"
    }
        
    macro dd(runtime typed o : java.math.BigInteger) with s : string {
      p(o: o) "BigInteger"
    }
    
    dd(o: c.someBigInteger) "ignored"
    """ controller(new DummyController) compilesTo
    <view><p o="5">BigInteger</p></view>
  }
}
