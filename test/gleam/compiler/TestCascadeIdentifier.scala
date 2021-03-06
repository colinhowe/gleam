package gleam.compiler

import gleam.compiler.node._
import gleam.Generator
import gleam.compiler.typing.Type
import gleam.compiler.typing.SimpleType
import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.{ Set => MSet, Buffer }
import scala.collection.JavaConversions._

import org.junit.Test

import org.mockito.Matchers._
import org.mockito.Mockito._

import gleam.compiler.ArgumentSource._

class TestCascadeIdentifier extends AssertionsForJUnit {

  private val typeResolver = new TypeResolver(
      new TypeProvider,
      new MacroDefinitionProvider)
  
  private def createOwnerMacro(cascade : Boolean, stmts : Buffer[PStmt]) = {
    HMacroDefn(
      "owner",
      args = Buffer[PArgDefn](
        new AArgDefn(
          if (cascade) 
            Buffer[PModifier](new ACascadeModifier)
          else 
            Buffer[PModifier](),
          new AStringType,
          new TIdentifier("arg"),
          null
        )
      ),
      withDefn = new AWithDefn(
        new TIdentifier("content"),
        new AStringType(),
        Buffer[PArgDefn]()
      ),
      generator = new AGenerator(
        Buffer[PArgDefn](),
        stmts
      )
    )
        
        
        
//        null,
//        new TIdentifier("owner"), 
//        Buffer[PGenericDefn](), 
//        Buffer[PArgDefn](
//            new AArgDefn(
//                if (cascade) 
//                  Buffer[PModifier](new ACascadeModifier)
//                else 
//                  Buffer[PModifier](),
//                new AStringType,
//                new TIdentifier("arg"),
//                null
//            )
//        ), 
//        new AWithDefn(
//          new TIdentifier("content"),
//          new AStringType(),
//          Buffer[PArgDefn]()
//        ),
//        null,
//        new AGenerator(
//            Buffer[PArgDefn](),
//            stmts
//        )
//    )
  }
  
  @Test
  def cascadeInOwningMacro = {
    val incrementStmt = new AIncrementStmt(new TIdentifier("toIncrement"))
    val defn = createOwnerMacro(true, Buffer[PStmt](incrementStmt))

    // Check what cascades are applicable
    val cascades = CascadeIdentifier.identify(incrementStmt, typeResolver, null, null)
    assert(1 === cascades.size)
    assert(SimpleType(classOf[String]) === cascades("arg"))
  }
  
  @Test
  def cascadeInMacroStmt = {
    val incrementStmt = new AIncrementStmt(new TIdentifier("toIncrement"))
    val macroStmt = new AMacroStmt(
        new TIdentifier("macro"),
        Buffer[PArgument](
            new AArgument(new TIdentifier("readonly"), new ATrueExpr())
        ),
        new AGeneratorExpr(new AGenerator(
            Buffer[PArgDefn](),
            Buffer[PStmt](incrementStmt)
        ))
    )
    
    val resolvedCallsProvider = new ResolvedCallsProvider
    val defn = new MacroDefinition("macro", new SimpleType(classOf[String]), false, Set[Restriction](), 
        Map[String, ArgumentDefinition]("readonly" -> ArgumentDefinition("readonly", new SimpleType(classOf[Boolean]), true, false)))
    
    val call = ResolvedCall(defn, Map("readonly" -> Cascade))
    
    resolvedCallsProvider.add(macroStmt, call)

    // Check what cascades are applicable
    val cascades = CascadeIdentifier.identify(incrementStmt, typeResolver, null, resolvedCallsProvider)
    assert(1 === cascades.size)
    assert(SimpleType(classOf[Boolean]) === cascades("readonly"))
  }
  
  @Test
  def innerCascadeHasPriorityInMacroStmt = {
    val resolvedCallsProvider = new ResolvedCallsProvider
    
    val incrementStmt = new AIncrementStmt(new TIdentifier("toIncrement"))
    val innerMacroStmt = new AMacroStmt(
        new TIdentifier("innermacro"),
        Buffer[PArgument](
            new AArgument(new TIdentifier("readonly"), new ATrueExpr())
        ),
        new AGeneratorExpr(new AGenerator(
            Buffer[PArgDefn](),
            Buffer[PStmt](incrementStmt)
        ))
    )
    val innerDefn = new MacroDefinition("innermacro", new SimpleType(classOf[String]), false, Set[Restriction](),
        Map("readonly" -> ArgumentDefinition("readonly", new SimpleType(classOf[Boolean]), true, false)))
    val innerCall = ResolvedCall(innerDefn, Map("readonly" -> Cascade))
    resolvedCallsProvider.add(innerMacroStmt, innerCall)
    
    val outerMacroStmt = new AMacroStmt(
        new TIdentifier("outermacro"),
        Buffer[PArgument](
            new AArgument(new TIdentifier("readonly"), new AStringExpr())
        ),
        new AGeneratorExpr(new AGenerator(
            Buffer[PArgDefn](),
            Buffer[PStmt](innerMacroStmt)
        ))
    )

    val outerDefn = new MacroDefinition("outermacro", new SimpleType(classOf[String]), false, Set[Restriction](),
        Map("readonly" -> ArgumentDefinition("readonly", new SimpleType(classOf[String]), true, false)))
    val outerCall = ResolvedCall(outerDefn, Map("readonly" -> Cascade))
    resolvedCallsProvider.add(outerMacroStmt, outerCall)

    // Check what cascades are applicable
    val cascades = CascadeIdentifier.identify(incrementStmt, typeResolver, null, resolvedCallsProvider)
    assert(1 === cascades.size)
    assert(SimpleType(classOf[Boolean]) === cascades("readonly"))
  }
  
  @Test
  def cascadeInMacroStmtInsideOwningMacro = {
    val resolvedCallsProvider = new ResolvedCallsProvider
    
    val incrementStmt = new AIncrementStmt(new TIdentifier("toIncrement"))
    val innerMacroStmt = new AMacroStmt(
        new TIdentifier("innermacro"),
        Buffer[PArgument](
            new AArgument(new TIdentifier("arg"), new ATrueExpr())
        ),
        new AGeneratorExpr(new AGenerator(
            Buffer[PArgDefn](),
            Buffer[PStmt](incrementStmt)
        ))
    )
    val innerDefn = new MacroDefinition("innermacro", new SimpleType(classOf[String]), false, Set[Restriction](),
        Map("arg" -> ArgumentDefinition("arg", new SimpleType(classOf[Boolean]), true, false)))
    val innerCall = ResolvedCall(innerDefn, Map("readonly" -> Cascade))
      resolvedCallsProvider.add(innerMacroStmt, innerCall)
    
    val defn = createOwnerMacro(true, Buffer[PStmt](innerMacroStmt))

    // Check what cascades are applicable
    val cascades = CascadeIdentifier.identify(incrementStmt, typeResolver, null, resolvedCallsProvider)
    assert(1 === cascades.size)
    assert(SimpleType(classOf[Boolean]) === cascades("arg"))
  }
  
  @Test
  def cascadeToOuterMacroStmt = {
    val resolvedCallsProvider = new ResolvedCallsProvider
    
    val incrementStmt = new AIncrementStmt(new TIdentifier("toIncrement"))
    val innerMacroStmt = new AMacroStmt(
        new TIdentifier("innermacro"),
        Buffer[PArgument](
            new AArgument(new TIdentifier("readonly"), new ATrueExpr())
        ),
        new AGeneratorExpr(new AGenerator(
            Buffer[PArgDefn](),
            Buffer[PStmt](incrementStmt)
        ))
    )
    val innerDefn = new MacroDefinition("innermacro", new SimpleType(classOf[String]), false, Set[Restriction](),
        Map("readonly" -> ArgumentDefinition("readonly", new SimpleType(classOf[Boolean]), false, false)))
    val innerCall = ResolvedCall(innerDefn, Map("readonly" -> Call))
    resolvedCallsProvider.add(innerMacroStmt, innerCall)
    
    val outerMacroStmt = new AMacroStmt(
        new TIdentifier("outermacro"),
        Buffer[PArgument](
            new AArgument(new TIdentifier("readonly"), new AStringExpr())
        ),
        new AGeneratorExpr(new AGenerator(
            Buffer[PArgDefn](),
            Buffer[PStmt](innerMacroStmt)
        ))
    )

    val outerDefn = new MacroDefinition("outermacro", new SimpleType(classOf[String]), false, Set[Restriction](),
        Map("readonly" -> ArgumentDefinition("readonly", new SimpleType(classOf[String]), true, false)))
    val outerCall = ResolvedCall(outerDefn, Map("readonly" -> Cascade))
    resolvedCallsProvider.add(outerMacroStmt, outerCall)

    // Check what cascades are applicable
    val cascades = CascadeIdentifier.identify(incrementStmt, typeResolver, null, resolvedCallsProvider)
    assert(1 === cascades.size)
    assert(SimpleType(classOf[String]) === cascades("readonly"))
  }
  
  @Test
  def noCascadeInOwningMacro = {
    val incrementStmt = new AIncrementStmt(new TIdentifier("toIncrement"))
    val defn = createOwnerMacro(false, Buffer[PStmt](incrementStmt))

    // Check what cascades are applicable
    val cascades = CascadeIdentifier.identify(incrementStmt, typeResolver, null, null)
    assert(0 === cascades.size)
  }
}