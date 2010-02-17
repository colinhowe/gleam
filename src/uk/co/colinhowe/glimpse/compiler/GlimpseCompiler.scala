package uk.co.colinhowe.glimpse.compiler
import java.io.BufferedReader
import java.io.File
import java.io.PushbackReader
import java.io.StringReader

import scala.collection.JavaConversions._
import scala.collection.JavaConversions
import uk.co.colinhowe.glimpse.CompilationError
import uk.co.colinhowe.glimpse.CompilationResult
import uk.co.colinhowe.glimpse.compiler.lexer.Lexer
import uk.co.colinhowe.glimpse.compiler.node.Start
import uk.co.colinhowe.glimpse.compiler.parser.Parser

class GlimpseCompiler {
  
  private def compileAsts(intermediates : List[IntermediateResult], classPaths : List[String]) : scala.collection.mutable.Buffer[CompilationResult] = {
    val results = scala.collection.mutable.Buffer[CompilationResult]()
    
    /*
     * Stage 1
     * Matching line numbers to nodes
     * Typing of nodes
     * Finding macro definitions
     */
    val lineNumberProvider = new LineNumberProvider()
    val typeProvider = new TypeProvider()
    val macroProvider = new MacroDefinitionProvider()
    val typeResolver = new TypeResolver(typeProvider, macroProvider)
    val classPathResolver = new ClassPathResolver(classPaths.toArray)
    val callResolver = new CallResolver(macroProvider)
    
    for (intermediate <- intermediates) {
      val ast = intermediate.ast
      val typeNameResolver = new TypeNameResolver(ast, classPathResolver)

      ast.apply(lineNumberProvider)
      
      val finder = new MacroDefinitionFinder(lineNumberProvider, typeProvider, macroProvider, typeNameResolver)
      ast.apply(finder)
      intermediate.errors ++ finder.errors

      ast.apply(typeResolver)
    }
    
    /*
     * Stage 2
     * Check type safety
     * Produce byte code
     * 
     * TODO Bundle all java files together and output in one go
     */
    for (intermediate <- intermediates) {
      
      val viewname = intermediate.viewName.replaceAll("-", "_")
      val ast = intermediate.ast
      
      val errors = scala.collection.mutable.Buffer[CompilationError]() ++ intermediate.errors
      
      // Run the type checker
      val typeNameResolver = new TypeNameResolver(ast, classPathResolver)
      val typeChecker = new TypeChecker(lineNumberProvider, macroProvider, typeResolver, typeNameResolver, callResolver)
      ast.apply(typeChecker)
      errors ++ typeChecker.errors
      
      // Compile all the nodes down to java
      val bcp = new ByteCodeProducer(
          viewname, 
          lineNumberProvider, 
          typeResolver, 
          "temp/" + viewname + ".class", 
          typeNameResolver, 
          intermediate.sourcename,
          callResolver)
      ast.apply(bcp)
      errors ++ bcp.errors
      
      // Output the errors
      for (error <- errors) {
        System.out.println(error)
      }
      
      // Output the view as a file only if needed
      val file = new File("temp/" + viewname + ".class")
      val result = new CompilationResult(viewname, file)
      for (error <- errors) {
        result.addError(error)
      }
      results += result
    }

    return results
  }

  def compile(units : java.util.List[CompilationUnit]) : java.util.List[CompilationResult] = {
    compile(units, new java.util.LinkedList[String]())
  }
  
  def compile(units : java.util.List[CompilationUnit], classPaths : java.util.List[String]) : java.util.List[CompilationResult] = {

    // Parse each source file
    val intermediates = scala.collection.mutable.Buffer[IntermediateResult]()
    
    for (unit <- units) {
      // create lexer
      val lexer = new Lexer(new PushbackReader(new BufferedReader(new StringReader(unit.getSource())), 204800))
      
      // parse program
      val parser = new Parser(lexer)
      val ast = parser.parse()
      
      intermediates.add(new IntermediateResult(ast, unit.getViewName(), unit.getSourceName))
    }

    // Compile the programs
    val classPathsBuffer : scala.collection.mutable.Buffer[String] = JavaConversions.asBuffer(classPaths) 
    return compileAsts(intermediates.toList, classPathsBuffer.toList)
  }
}
