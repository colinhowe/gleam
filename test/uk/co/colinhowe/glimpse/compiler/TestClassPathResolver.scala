package uk.co.colinhowe.glimpse.compiler

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import scala.reflect.BeanProperty

class TestClassPathResolver extends AssertionsForJUnit {
  
  @Test
  def testFolder = {
    val resolver = new ClassPathResolver(Array[String]("bin"))
    
    assert(true === resolver.isClassOnClassPath("uk.co.colinhowe.glimpse.Node"))
  }
  
  @Test
  def testJar = {
    val resolver = new ClassPathResolver(Array[String]("bin/uk/co/colinhowe/glimpse/compiler/node.jar"))
    
    assert(true === resolver.isClassOnClassPath("uk.co.colinhowe.glimpse.Node"))
  }
}