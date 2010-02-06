package uk.co.colinhowe.glimpse

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import scala.reflect.BeanProperty

class TestPropertyReference extends AssertionsForJUnit {
  
  class DummyController(var name : String) {
    var inner : DummyController = null
    def getInner = inner
    
    def setName(v : String) {
      name = v
    }
  }
  
  @Test
  def testSimple = {
    val controller = new DummyController("harry")
    val ref = new PropertyReference("name", controller.name)
    assert("harry" === ref.value)

    ref.set(controller, "bob")
    assert("bob" === controller.name)
  }
  
  @Test
  def testComplex = {
    val parent = new DummyController("parent")
    val child = new DummyController("child")
    parent.inner = child
    
    val ref = new PropertyReference("inner.name", child.name)
    assert("child" === ref.value)

    ref.set(parent, "inside")
    assert("inside" === child.name)
    assert("parent" === parent.name)
  }
}