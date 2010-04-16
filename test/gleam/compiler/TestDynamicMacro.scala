package gleam.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import gleam.View
import gleam.Node
import gleam.DynamicMacroMismatchError

import org.junit.Assert._

class TestDynamicMacro extends TypeCheckerTest {
  
  @Test
  def noArguments = {  
    """
    dynamic macro dynamo with g : generator 
    node d with generator
    node p with string
    macro div with g : generator {
      d {
        include g
      }   
    }
    
    dynamo = div
    
    dynamo {
      p "Inside"
    }
    """ compilesTo <view>
      <d>
        <p>Inside</p>
      </d>
    </view>
  }
    
  @Test
  def arguments = {   
    """
    dynamic macro dynamo(value : int) with s : string 
    node x(value : int) with string
    macro p(value : int) with s : string {
      x(value: value) s   
    }
    
    dynamo = p
    
    dynamo(value: 2) "hi"
    """ compilesTo <view>
      <x value="2">hi</x>
    </view>
  }
    
  @Test
  def valueNameMismatchOk = {   
    """
    node x with string
    dynamic macro dynamo with str : string
    macro p with s : string {
      x s
    }
    
    dynamo = p
    
    dynamo "hi"
    """ compilesTo <view>
      <x>hi</x>
    </view>
  }
    
  @Test
  def valueMismatch = {   
    """
    dynamic macro dynamo with g : generator 
    macro p with s : string {
      var x = 1
    }
    
    dynamo = p
    
    dynamo {
      var x = 1
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
      var x = 1
    }
    
    dynamo = p
    
    dynamo(value: 2) "hi"
    """ failsWith
    DynamicMacroMismatchError(
        line = 7, 
        dynamicMacro = "dynamo") // TODO Add some details to this error
  }
}