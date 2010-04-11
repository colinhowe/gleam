package uk.co.colinhowe.gleam.compiler

import java.lang.reflect.Method
import uk.co.colinhowe.gleam.Generator
import uk.co.colinhowe.gleam.compiler.typing.Type
import uk.co.colinhowe.gleam.compiler.typing.SimpleType
import org.scalatest.junit.AssertionsForJUnit
import uk.co.colinhowe.gleam.compiler.node._
import scala.collection.mutable.{ Set => MSet, Buffer }
import scala.collection.JavaConversions._

import org.junit.Test

import org.mockito.Matchers._
import org.mockito.Mockito._
import uk.co.colinhowe.gleam.compiler.ArgumentSource._

class TestMethodResolver extends AssertionsForJUnit {
  
  @Test
  def methodWithoutArguments = {
    val resolver = new MethodResolver(null, null)
    
    val method = resolver.getMatchingMethod(classOf[DummyController], new AControllerMethodExpr(
      new TIdentifier("getName"),
      Buffer[PExpr]()
    )).get
    
    assert(classOf[String] === method.getReturnType)
  }
  
  @Test
  def methodOnController = {
    val typeResolver = mock(classOf[TypeResolver])
    when(typeResolver.getType(any(), any(), any())).thenReturn(SimpleType(classOf[String]))
    val resolver = new MethodResolver(typeResolver, null)
    
    val method = resolver.getMatchingMethod(classOf[DummyController], new AControllerMethodExpr(
      new TIdentifier("makeMessage"),
      Buffer[PExpr](new AStringExpr(new TString("ignored")))
    )).get
    
    assert(classOf[String] === method.getReturnType)
    assert(classOf[String] === method.getParameterTypes()(0))
  }
}