package uk.co.colinhowe.gleam.compiler

import org.junit.Test
import java.io.File
import java.net.URLClassLoader
import java.net.URL
import uk.co.colinhowe.gleam.View
import uk.co.colinhowe.gleam.Node

import org.junit.Assert._

class TestGenerics extends CompilerTest {
  
  @Test
  def simpleGenerics = {   
    """
    node x<T>(value: T) with string
    macro p<T>(value : T) with s : string {
      x(value: value) s
    }

    p(value: "a") "hi1"
    p(value: 1) "hi2"
    """ compilesTo 
    <view><x value="a">hi1</x><x value="1">hi2</x></view>
  }
  
  @Test
  def listGenerics = {   
    """
    controller uk.co.colinhowe.gleam.compiler.DummyController
    
    node p with string
    macro result_set<T>(list : java.util.List<T>) with g : generator(row : T) {
      for (T t in list) {
        include g(row: t)
      }
    }

    result_set(list: c.names) { row : string =>
      p row
    }
    """ controller(new DummyController) compilesTo 
    <view><p>Alan</p><p>Bob</p><p>Colin</p></view>
  }
  
  @Test
  def listGenericsInteger = {   
    """
    controller uk.co.colinhowe.gleam.compiler.DummyController
    
    node p with int
    macro result_set<T>(list : java.util.List<T>) with g : generator(row : T) {
      for (T t in list) {
        include g(row: t)
      }
    }
   
    result_set(list: c.ages) { row : int =>
      p row
    }
    """ controller(new DummyController) compilesTo 
    <view><p>18</p><p>21</p><p>25</p></view>
  }
  
}