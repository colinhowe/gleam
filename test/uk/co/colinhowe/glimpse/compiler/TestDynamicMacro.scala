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
    dynamic macro dynamo with g : generator 
    macro div with g : generator {
      node div {
        include g
      }   
    }
    
    dynamo = div
    
    dynamo {
      node p "Inside"
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
    dynamic macro dynamo(value : int) with s : string 
    macro p(value : int) with s : string {
      node p(value: value) s   
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
    dynamic macro dynamo with str : string
    macro p with s : string {
      node p s
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
    dynamic macro dynamo with g : generator 
    macro p with s : string {
      node p s   
    }
    
    dynamo = p
    
    dynamo {
      node p "hi"
    }
    """ failsWith
    DynamicMacroMismatchError(
        line = 7, 
        dynamicMacro = "dynamo") // TODO Add some details to this error
  }
    
  @Test
  def argumentMismatch = {   
    """
    dynamic macro dynamo(value : int) with s : string 
    macro p with s : string {
      node p s   
    }
    
    dynamo = p
    
    dynamo(value: 2) "hi"
    """ failsWith
    DynamicMacroMismatchError(
        line = 7, 
        dynamicMacro = "dynamo") // TODO Add some details to this error
  }

}