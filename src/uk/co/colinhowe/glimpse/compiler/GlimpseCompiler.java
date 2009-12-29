package uk.co.colinhowe.glimpse.compiler;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PushbackReader;
import java.io.StringReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicInteger;

import uk.co.colinhowe.glimpse.CompilationError;
import uk.co.colinhowe.glimpse.CompilationResult;
import uk.co.colinhowe.glimpse.TypeCheckError;
import uk.co.colinhowe.glimpse.compiler.lexer.Lexer;
import uk.co.colinhowe.glimpse.compiler.node.AConstantExpr;
import uk.co.colinhowe.glimpse.compiler.node.AGenerator;
import uk.co.colinhowe.glimpse.compiler.node.AQualifiedName;
import uk.co.colinhowe.glimpse.compiler.node.ASimpleName;
import uk.co.colinhowe.glimpse.compiler.node.PExpr;
import uk.co.colinhowe.glimpse.compiler.node.PName;
import uk.co.colinhowe.glimpse.compiler.node.Start;
import uk.co.colinhowe.glimpse.compiler.parser.Parser;

public class GlimpseCompiler {
  
  public CompilationResult compile(String viewname, Start ast) {
    final List<CompilationError> errors = new LinkedList<CompilationError>();
    
    final LineNumberProvider lineNumberProvider = new LineNumberProvider();
    ast.apply(lineNumberProvider);
    
    final TypeProvider typeProvider = new TypeProvider();
    ast.apply(typeProvider);
 
    final CallResolver callResolver = new CallResolver(lineNumberProvider, typeProvider);
    ast.apply(callResolver);
    
    // Run the type checker
    final TypeChecker typeChecker = new TypeChecker(lineNumberProvider, callResolver, typeProvider);
    ast.apply(typeChecker);
    errors.addAll(typeChecker.getErrors());
    
    // Create the java source to compile
    final StringBuffer javaSource = new StringBuffer();
    final List<String> methods = new LinkedList<String>();
    final Stack<StringBuffer> buffers = new Stack<StringBuffer>();
    final AtomicInteger generatorCount = new AtomicInteger();
    final Map<AGenerator, Integer> generatorIds = new HashMap<AGenerator, Integer>();
    final Set<String> macroDefns = new HashSet<String>();
    final Set<String> currentMacroArguments = new HashSet<String>();
    
    // Start the class for the view
    viewname = viewname.replaceAll("-", "_");
    javaSource.append("import java.util.List;\n");
    javaSource.append("import java.util.LinkedList;\n");
    javaSource.append("import java.util.Map;\n");
    javaSource.append("import java.util.HashMap;\n");
    javaSource.append("import uk.co.colinhowe.glimpse.Node;\n");
    javaSource.append("import uk.co.colinhowe.glimpse.View;\n");
    javaSource.append("import uk.co.colinhowe.glimpse.Generator;\n");
    javaSource.append("import java.util.Map.Entry;\n");
    javaSource.append("public class " + viewname + " implements View {\n");

    // Create the view
    buffers.add(new StringBuffer());
    
    // Create the initial environment for variables
    buffers.peek().append("  private Map<String, Object> environment = new HashMap<String, Object>();\n");

    // Start the view method
    buffers.peek().append("  public List<Node> view() {\n");
    buffers.peek().append("    List<Node> nodes = new LinkedList<Node>();\n");
    
    final StringBuffer controllerType = new StringBuffer();
    
    // Compile all the nodes down to java
    ast.apply(new ByteCodeProducer(generatorCount, methods, buffers, generatorIds,
        currentMacroArguments, macroDefns, controllerType));
    
    // End the view method
    buffers.peek().append("    return nodes;\n");
    buffers.peek().append("  }\n");
    
    // Push the final buffer onto the method list
    methods.add(buffers.pop().toString());
    
    // Output all the methods
    for (String method : methods) {
      javaSource.append(method);
    }
    
    // Output all the macros
    for (String macroDefn : macroDefns) {
      javaSource.append(macroDefn);
    }
    
    // Output a constructor if needed
    if (controllerType.length() != 0) {
      javaSource.append("  final private " + controllerType + " controller;\n");
      javaSource.append("  public " + viewname + "(final " + controllerType + " controller) {\n");
      javaSource.append("    this.controller = controller;\n");
      javaSource.append("  }\n");
    }
    
    // End the class
    javaSource.append("}\n");

    // Output the class with line counters
    int i = 1;
    for (String line : javaSource.toString().split("\n")) {
      System.out.println(String.format("%03d %s", i++, line));
    }
    
//    System.out.println(javaSource);
    
    // Output the source to a temporary file
    try {
      File file = new File(viewname + ".java");
      file.deleteOnExit();
      
      BufferedWriter writer = new BufferedWriter(new FileWriter(file));
      writer.write(javaSource.toString());
      writer.close();
      
      // Compile the code
      int errorCode = com.sun.tools.javac.Main.compile(new String[] {
          "-classpath", "bin",
          "-d", "temp",
          file.getAbsolutePath() });
      
      File clazz = new File("temp/" + viewname + ".class");
      clazz.deleteOnExit();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }

    final CompilationResult result = new CompilationResult(viewname);
    for (final CompilationError error : errors) {
      result.addError(error);
    }
    return result;
  }

  public CompilationResult compile(String source) {
    // create lexer
    Lexer lexer = new Lexer (new PushbackReader(new BufferedReader(new StringReader(source))));
    
    // parse program
    Parser parser = new Parser(lexer);

    Start ast;
    try {
      ast = parser.parse();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }

    // Compile the program
    return compile("basic-string", ast);
  }
}
