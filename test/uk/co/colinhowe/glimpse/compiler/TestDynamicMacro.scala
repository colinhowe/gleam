package uk.co.colinhowe.glimpse.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node
import uk.co.colinhowe.glimpse.DynamicMacroMismatchError

import org.junit.Assert._

class TestDynamicMacro extends TypeCheckerTest {
  
  @Test
  def noArguments = {  
    
    """
    dynamic macro dynamo with generator g 
    macro div with generator g {
      node:div {
        include g
      }   
    }
    
    dynamo = div
    
    dynamo {
      node:p "Inside"
    }
    """ compilesTo <view>
      <div>
        <p>Inside</p>
      </div>
    </view>
  }
    
  @Test
  def arguments = {   
    """
    dynamic macro dynamo(int value) with string s 
    macro p(int value) with string s {
      node:p(value: value) s   
    }
    
    dynamo = p
    
    dynamo(value: 2) "hi"
    """ compilesTo <view>
      <p value="2">hi</p>
    </view>
  }
    
  @Test
  def valueNameMismatchOk = {   
    """
    dynamic macro dynamo with string str
    macro p with string s {
      node:p s
    }
    
    dynamo = p
    
    dynamo "hi"
    """ compilesTo <view>
      <p>hi</p>
    </view>
  }
    
  @Test
  def valueMismatch = {   
    """
    dynamic macro dynamo with generator g 
    macro p with string s {
      node:p s   
    }
    
    dynamo = p
    
    dynamo "hi"
    """ failsWith
    DynamicMacroMismatchError(
        line = 7, 
        macro = "p",
        dynamicMacro = "dynamo") // TODO Add some details to this error
  }
    
  @Test
  def argumentMismatch = {   
    """
    dynamic macro dynamo(int value) with string s 
    macro p with string s {
      node:p s   
    }
    
    dynamo = p
    
    dynamo(value: 2) "hi"
    """ failsWith
    DynamicMacroMismatchError(
        line = 7, 
        macro = "p",
        dynamicMacro = "dynamo") // TODO Add some details to this error
  }

}