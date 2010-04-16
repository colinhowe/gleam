package gleam.compiler

import java.lang.reflect.Method
import gleam.Generator
import gleam.compiler.typing.Type
import gleam.compiler.typing.SimpleType
import org.scalatest.junit.AssertionsForJUnit
import gleam.compiler.node._
import scala.collection.mutable.{ Set => MSet, Buffer }
import scala.collection.JavaConversions._

import org.junit.Test

import org.mockito.Matchers._
import org.mockito.Mockito._
import gleam.compiler.ArgumentSource._

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