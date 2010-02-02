package uk.co.colinhowe.glimpse.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.glimpse.View
import uk.co.colinhowe.glimpse.Node

import org.junit.Assert._

class TestGenerics extends CompilerTest {
  
  @Test
  def simpleGenerics = {   
    """
    macro p<T>(T value) with string s {
      node p(value: value) s
    }

    p(value: "a") "hi1"
    p(value: 1) "hi2"
    """ compilesTo 
    <view><p value="a">hi1</p><p value="1">hi2</p></view>
  }
  
  @Test
  def listGenerics = {   
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    
    macro result_set<T>(java.util.List<T> list) with generator g(T row) {
      for (T t in list) {
        include g(row: t)
      }
    }

    result_set(list: c.names) { string row =>
      node p row
    }
    """ controller(new DummyController) compilesTo 
    <view><p>Alan</p><p>Bob</p><p>Colin</p></view>
  }
  
  @Test
  def listGenericsInteger = {   
    """
    controller uk.co.colinhowe.glimpse.compiler.DummyController
    
    macro result_set<T>(java.util.List<T> list) with generator g(T row) {
      for (T t in list) {
        include g(row: t)
      }
    }

    result_set(list: c.ages) { int row =>
      node p row
    }
    """ controller(new DummyController) compilesTo 
    <view><p>18</p><p>21</p><p>25</p></view>
  }
  
}