package uk.co.colinhowe.glimpse.feature

import uk.co.colinhowe.glimpse.compiler.DummyController
import org.junit.Test
import org.junit.Assert._

import uk.co.colinhowe.glimpse.compiler.typing.Type
import uk.co.colinhowe.glimpse.MacroNotFoundError
import uk.co.colinhowe.glimpse.compiler.TypeCheckerTest
import uk.co.colinhowe.glimpse.compiler.MacroDefinition
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType
import uk.co.colinhowe.glimpse.compiler.Restriction

class TestDoubleDispatch extends TypeCheckerTest {
  
  @Test
  def actualClass = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
      
    abstract macro dd(runtime typed o : java.lang.Object) with s : string
        
    macro dd(runtime typed o : string) with s : string {
      node dd(o: o) "Success"      
    }      
    
    dd(o: c.someString) "ignored"
    """ controller(new DummyController) compilesTo
    <view><dd o="someString">Success</dd></view>
  }
  
  @Test
  def parentClass = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
      
    abstract macro dd(runtime typed o : java.lang.Object) with s : string
        
    macro dd(runtime typed o : java.lang.Number) with s : string {
      node dd(o: o) "Success"
    }      
    
    dd(o: c.someBigInteger) "ignored"
    """ controller(new DummyController) compilesTo
    <view><dd o="5">Success</dd></view>
  }
  
  @Test
  def bestMatch = {
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
      
    abstract macro dd(runtime typed o : java.lang.Object) with s : string
        
    macro dd(runtime typed o : java.lang.Number) with s : string {
      node dd(o: o) "Number"
    }
        
    macro dd(runtime typed o : java.math.BigInteger) with s : string {
      node dd(o: o) "BigInteger"
    }
    
    dd(o: c.someBigInteger) "ignored"
    """ controller(new DummyController) compilesTo
    <view><dd o="5">BigInteger</dd></view>
  }
}
