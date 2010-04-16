package gleam.infrastructure

import gleam.IdentifierNotFoundException
import gleam.Macro
import gleam.Node

import scala.collection.mutable.{ Buffer, Map => MMap }
import scala.collection.JavaConversions._
import org.scalatest.junit.AssertionsForJUnit

import org.junit.Test
import org.junit.Before

import java.math.BigInteger
import java.io.File
import java.net.URLClassLoader

object RttImpl {
  var invocationCount = 0
  def getInstance() = new RttImpl
}

/**
 * Dummy implementation of a macro
 */
class RttImpl extends Macro {
  def invoke(scope : Scope, args : java.util.Map[String, Object], value : Object) : java.util.List[Node] = {
    RttImpl.invocationCount += 1
    null
  }
}

class MockRuntimeTypedMacro extends RuntimeTypedMacro {
  def getRuntimeTypedArgumentName = "o"
  def getMacroName = "mockRtt"
  var result : Map[String, Class[_]] = null
  var attemptCount = 0
  
  override def attemptClassWithName(className : String) = {
    attemptCount += 1
    result.get(className) match {
      case Some(x) => x
      case None => null
    }
  }
}

class TestRuntimeTypedMacro extends AssertionsForJUnit {
  
  @Before
  def resetCounts {
    RttImpl.invocationCount = 0
  }
  
  @Test
  def actualClassImplementationExists() {
    val macro = new MockRuntimeTypedMacro
    macro.result = Map("mockRtt$rtt473e3665" -> classOf[RttImpl]) // Hash of String
    macro.invoke(null, MMap[String, Object]("o" -> "someString"), "value")
    assert(1 === RttImpl.invocationCount)
  }
  
  @Test
  def parentClassImplementationExists() {
    val macro = new MockRuntimeTypedMacro
    macro.result = Map("mockRtt$rtt3ec1b19d" -> classOf[RttImpl]) // Hash of Number
    macro.invoke(null, MMap[String, Object]("o" -> new BigInteger("5")), "value")
    assert(1 === RttImpl.invocationCount)
  }
  
  @Test
  def concreteImplementationMissing() {
    val macro = new MockRuntimeTypedMacro
    macro.result = Map() // Hash of Number
    
    val exception = intercept[RuntimeException] { 
      macro.invoke(null, MMap[String, Object]("o" -> new BigInteger("5")), "value")
    }
    assert("No concrete implementation of mockRtt with runtime type java.math.BigInteger exists for argument o" === exception.getMessage)
  }
  
  @Test
  def cacheRuntimeMatch() {
    val macro = new MockRuntimeTypedMacro
    macro.result = Map("mockRtt$rtt473e3665" -> classOf[RttImpl]) // Hash of String
    macro.invoke(null, MMap[String, Object]("o" -> "someString"), "value")
    macro.invoke(null, MMap[String, Object]("o" -> "someString"), "value")

    assert(1 === macro.attemptCount)
  }
}

