package uk.co.colinhowe.glimpse.compiler;

import java.io.BufferedReader;
import java.io.File;
import java.io.PushbackReader;
import java.io.StringReader;
import java.util.LinkedList;
import java.util.List;

import uk.co.colinhowe.glimpse.CompilationError;
import uk.co.colinhowe.glimpse.CompilationResult;
import uk.co.colinhowe.glimpse.compiler.lexer.Lexer;
import uk.co.colinhowe.glimpse.compiler.node.Start;
import uk.co.colinhowe.glimpse.compiler.parser.Parser;

public class GlimpseCompiler {
  
  private List<CompilationResult> compileAsts(List<IntermediateResult> intermediates, List<String> classPaths) {
    List<CompilationResult> results = new LinkedList<CompilationResult>();
    
    /*
     * Stage 1
     * Matching line numbers to nodes
     * Typing of nodes
     * Finding macro definitions
     */
    final LineNumberProvider lineNumberProvider = new LineNumberProvider();
    final TypeProvider typeProvider = new TypeProvider();
    final MacroDefinitionProvider macroProvider = new MacroDefinitionProvider();
    
    for (IntermediateResult intermediate : intermediates) {
      Start ast = intermediate.ast;
    
      ast.apply(lineNumberProvider);
      ast.apply(typeProvider);
      
      MacroDefinitionFinder finder = new MacroDefinitionFinder(typeProvider, lineNumberProvider, macroProvider);
      ast.apply(finder);
      intermediate.errors.addAll(finder.errorsAsJavaList());
    }
    
    /*
     * Stage 2
     * Check type safety
     * Produce byte code
     * 
     * TODO Bundle all java files together and output in one go
     */
    for (IntermediateResult intermediate : intermediates) {
      
      String viewname = intermediate.viewName;
      Start ast = intermediate.ast;
      
      final List<CompilationError> errors = new LinkedList<CompilationError>(intermediate.errors);
      
      // Run the type checker
      final TypeChecker typeChecker = new TypeChecker(lineNumberProvider, macroProvider, typeProvider);
      ast.apply(typeChecker);
      errors.addAll(typeChecker.getErrors());
      
      // Start the class for the view
      viewname = viewname.replaceAll("-", "_");
  
      // Compile all the nodes down to java
      ast.apply(new ByteCodeProducer(viewname, lineNumberProvider, typeProvider, "temp/" + viewname + ".class"));
      
      // Output the view as a file only if needed
      File file = new File("temp/" + viewname + ".class");
      final CompilationResult result = new CompilationResult(viewname, file);
      for (final CompilationError error : errors) {
        result.addError(error);
      }
      results.add(result);
    }

    return results;
  }

  public List<CompilationResult> compile(List<CompilationUnit> units) {
    return compile(units, new LinkedList<String>());
  }
  
  public List<CompilationResult> compile(List<CompilationUnit> units, List<String> classPaths) {

    // Parse each source file
    List<IntermediateResult> intermediates = new LinkedList<IntermediateResult>();
    
    for (CompilationUnit unit : units) {
      // create lexer
      Lexer lexer = new Lexer(new PushbackReader(new BufferedReader(new StringReader(unit.getSource())), 204800));
      
      // parse program
      Parser parser = new Parser(lexer);

      Start ast;
      try {
        ast = parser.parse();
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
      
      intermediates.add(new IntermediateResult(ast, unit.getViewName()));
    }

    // Compile the programs
    return compileAsts(intermediates, classPaths);
  }
  
  private static class IntermediateResult {
    private Start ast;
    private String viewName;
    private List<CompilationError> errors = new LinkedList<CompilationError>();

    public IntermediateResult(Start ast, String viewName) {
      this.ast = ast;
      this.viewName = viewName;
    }
  }
}
