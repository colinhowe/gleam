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

import javax.management.RuntimeErrorException;

import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter;
import uk.co.colinhowe.glimpse.compiler.lexer.Lexer;
import uk.co.colinhowe.glimpse.compiler.node.AArgDefn;
import uk.co.colinhowe.glimpse.compiler.node.AArgument;
import uk.co.colinhowe.glimpse.compiler.node.AAssignmentStmt;
import uk.co.colinhowe.glimpse.compiler.node.AConstantExpr;
import uk.co.colinhowe.glimpse.compiler.node.AController;
import uk.co.colinhowe.glimpse.compiler.node.AControllerPropExpr;
import uk.co.colinhowe.glimpse.compiler.node.AGenerator;
import uk.co.colinhowe.glimpse.compiler.node.AGeneratorExpr;
import uk.co.colinhowe.glimpse.compiler.node.AGeneratorType;
import uk.co.colinhowe.glimpse.compiler.node.AIncludeA;
import uk.co.colinhowe.glimpse.compiler.node.AIncludeStmt;
import uk.co.colinhowe.glimpse.compiler.node.AIncrementStmt;
import uk.co.colinhowe.glimpse.compiler.node.AIntType;
import uk.co.colinhowe.glimpse.compiler.node.AMacroDefn;
import uk.co.colinhowe.glimpse.compiler.node.AMacroStmt;
import uk.co.colinhowe.glimpse.compiler.node.ANoInitVarDefn;
import uk.co.colinhowe.glimpse.compiler.node.ANodeCreate;
import uk.co.colinhowe.glimpse.compiler.node.APropertyExpr;
import uk.co.colinhowe.glimpse.compiler.node.AQualifiedName;
import uk.co.colinhowe.glimpse.compiler.node.ASimpleName;
import uk.co.colinhowe.glimpse.compiler.node.AStringExpr;
import uk.co.colinhowe.glimpse.compiler.node.AStringType;
import uk.co.colinhowe.glimpse.compiler.node.AVarDefnStmt;
import uk.co.colinhowe.glimpse.compiler.node.AWithGeneratorMacroInvoke;
import uk.co.colinhowe.glimpse.compiler.node.AWithInitVarDefn;
import uk.co.colinhowe.glimpse.compiler.node.AWithStringMacroInvoke;
import uk.co.colinhowe.glimpse.compiler.node.PArgDefn;
import uk.co.colinhowe.glimpse.compiler.node.PArgument;
import uk.co.colinhowe.glimpse.compiler.node.PExpr;
import uk.co.colinhowe.glimpse.compiler.node.PMacroInvoke;
import uk.co.colinhowe.glimpse.compiler.node.PName;
import uk.co.colinhowe.glimpse.compiler.node.PType;
import uk.co.colinhowe.glimpse.compiler.node.PVarDefn;
import uk.co.colinhowe.glimpse.compiler.node.Start;
import uk.co.colinhowe.glimpse.compiler.node.TString;
import uk.co.colinhowe.glimpse.compiler.parser.Parser;

public class GlimpseCompiler {
  
  public String getStringFromExpr(PExpr expr) {
    if (expr instanceof AConstantExpr) {
      return ((AConstantExpr)expr).getNumber().getText();
    }
    
    return "0";
  }
  
  public String nameToString(PName node) {
    // Chunk the name down
    PName nameNode = node;
    String name = "";
    List<String> parts = new LinkedList<String>();
    
    while (nameNode != null) {
      if (nameNode instanceof AQualifiedName) {
        name = "." + ((AQualifiedName)nameNode).getIdentifier().getText() + name;
        nameNode = ((AQualifiedName)nameNode).getName();
      } else {
        name = ((ASimpleName)nameNode).getIdentifier().getText() + name;
        nameNode = null;
      }
    }
    return name;
  }
  
  public String upperFirst(String other) {
    return other.substring(0, 1).toUpperCase() + other.substring(1);
  }
  
  public String nameToStringWithGets(PName node) {
    // Chunk the name down
    PName nameNode = node;
    String name = "";
    List<String> parts = new LinkedList<String>();
    
    while (nameNode != null) {
      if (nameNode instanceof AQualifiedName) {
        name = ".get" + upperFirst(((AQualifiedName)nameNode).getIdentifier().getText()) + "()" + name;
        nameNode = ((AQualifiedName)nameNode).getName();
      } else {
        name = "get" + upperFirst(((ASimpleName)nameNode).getIdentifier().getText()) + "()" + name;
        nameNode = null;
      }
    }
    return name;
  }
  
  public void compile(String viewname, Start ast) {
    
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
    ast.apply(new DepthFirstAdapter() {
      
      @Override
      public void caseTString(TString node) {
        node.setText(node.getText().replaceAll("\"", ""));
      }
      
      @Override
      public void inAMacroDefn(AMacroDefn node) {
        // Create arguments 
        for (PArgDefn pargDefn : node.getArgDefn()) {
          if (pargDefn instanceof AArgDefn) {
            AArgDefn argDefn = (AArgDefn)pargDefn;
            currentMacroArguments.add(argDefn.getIdentifier().getText());
          }
        }
      }
      
      @Override
      public void outAController(AController node) {
        String name = nameToString(node.getName());
        controllerType.append(name);
      }
      
      @Override
      public void outAMacroDefn(AMacroDefn node) {
        // TODO Extract these out into new java files so that the class can be shared
        // Generate the macro code
        final StringBuffer buffer = new StringBuffer();
        buffer.append("  private class macro" + node.getName().getText() + "{\n");
        
        String parameters = "Map<String, Object> _args, ";
        String environmentPuts = "";
        // TODO Finish parameters
        if (node.getContentType() instanceof AStringType) {
          parameters += "String " + node.getContentName().getText();
          environmentPuts = "      environment.put(\"" + node.getContentName().getText() + "\", " + node.getContentName().getText() + ");\n";
        } else if (node.getContentType() instanceof AGeneratorType) {
          parameters += "Generator " + node.getContentName().getText();
          environmentPuts = "      environment.put(\"" + node.getContentName().getText() + "\", " + node.getContentName().getText() + ");\n";
        }
        
        // TODO set the environment for the generator from the arguments
        
        // Simply call the generator with the parameters in scope
        buffer.append("    public List<Node> invoke(" + parameters + ") {\n");
        
        // Push the parameters onto the environment
        buffer.append(environmentPuts);
        
        // Push all the arguments onto the environment
        buffer.append("      for (Entry<String, Object> entry : _args.entrySet()) {\n");
        buffer.append("        environment.put(entry.getKey(), entry.getValue());\n");
        buffer.append("      }\n");
        
        int generatorId = generatorIds.get(node.getGenerator());
        
        buffer.append("      return new generator" + generatorId + "().view(new HashMap<String, Object>());\n");
        buffer.append("    }\n");
        
        buffer.append("  }\n");
        macroDefns.add(buffer.toString());
        
        // Bin all the arguments
        currentMacroArguments.clear();
      }
      
      @Override
      public void outAIncludeStmt(AIncludeStmt node) {
        StringBuffer buffer = buffers.peek();
        
        // Build up arguments for the generator
        buffer.append("      {\n");
        buffer.append("        Map<String, Object> genArgs = new HashMap<String, Object>();\n");
        
        for (PArgument parg : ((AIncludeA)node.getIncludeA()).getArguments()) {
          AArgument arg = (AArgument)parg;
          String expr = getStringFromExpr(arg.getExpr());
          buffer.append("        genArgs.put(\"" + arg.getIdentifier().getText() + "\", " + expr + ");\n");
        }
        
        String generatorName = ((AIncludeA)node.getIncludeA()).getTheInclude().getText();
        buffer.append("        nodes.addAll(((Generator)environment.get(\"" + generatorName + "\")).view(genArgs));\n");
        buffer.append("      }\n");
      }
      
      @Override
      public void inAGenerator(AGenerator node) {
        // Get the ID for the generator
        int id = generatorCount.getAndIncrement();
        generatorIds.put(node, id);
        
        // TODO Create environments for generators so that we can handle variables
        
        // Create the generator class
        buffers.add(new StringBuffer());
        buffers.peek().append("  private class generator" + id + " implements Generator {\n");
        buffers.peek().append("    public List<Node> view(Map<String, Object> args) {\n");
        buffers.peek().append("      List<Node> nodes = new LinkedList<Node>();\n");
      }
      
      @Override
      public void outAGenerator(AGenerator node) {
        buffers.peek().append("      return nodes;\n");
        buffers.peek().append("    }\n");
        buffers.peek().append("  }\n");
        methods.add(buffers.pop().toString());
      }
      
      @Override
      public void outAWithInitVarDefn(AWithInitVarDefn node) {
        
        PType type = node.getType();
        String typeString = "";
        if (type instanceof AIntType) {
          typeString = "int";
        } else if (type instanceof AStringType) {
          typeString = "String";
        } else if (type instanceof AGeneratorType) {
          throw new RuntimeException("Cannot declare a generator as a variable");
        }
        buffers.peek().append("      " + typeString + " " + node.getIdentifier().getText() + "=" + getStringFromExpr(node.getExpr()) + ";\n");
        
        String varname = "";
        varname = node.getIdentifier().getText();
        buffers.peek().append("    environment.put(\"" + varname + "\", " + varname + ");\n");
      }
      
      @Override
      public void outANoInitVarDefn(ANoInitVarDefn node) {

        PType type = node.getType();
        String typeString = "";
        String defaultValue = "";
        if (type instanceof AIntType) {
          typeString = "int";
          defaultValue = "0";
        } else if (type instanceof AStringType) {
          typeString = "String";
          defaultValue = "null";
        } else if (type instanceof AGeneratorType) {
          throw new RuntimeException("Cannot declare a generator as a variable");
        }
        buffers.peek().append("      " + typeString + " " + node.getIdentifier().getText() + "=" + defaultValue + ";\n");
        
        String varname = "";
        varname = node.getIdentifier().getText();
        buffers.peek().append("    environment.put(\"" + varname + "\", " + varname + ");\n");
      }
      
      @Override
      public void outAIncrementStmt(AIncrementStmt node) {
        String varName = node.getIdentifier().getText();
        buffers.peek().append("    {\n");
        buffers.peek().append("      int __a = (Integer)environment.get(\"" + varName + "\");\n");
        buffers.peek().append("      __a++;\n");
        buffers.peek().append("      environment.put(\"" + varName + "\", __a);\n");
        buffers.peek().append("    }\n");
        super.outAIncrementStmt(node);
      }
      
      @Override
      public void outAAssignmentStmt(AAssignmentStmt node) {
        String varName = node.getIdentifier().getText();
        buffers.peek().append("    {\n");
        buffers.peek().append("      int __a = (Integer)environment.get(\"" + varName + "\");\n");
        buffers.peek().append("      __a = " + getStringFromExpr(node.getExpr()));
        buffers.peek().append("      environment.put(\"" + varName + "\", __a);\n");
        buffers.peek().append("    }\n");
      }
      
      @Override
      public void outAMacroStmt(AMacroStmt node) {
        StringBuffer buffer = buffers.peek();
        PMacroInvoke invocation = node.getMacroInvoke();
        
        if (invocation instanceof AWithStringMacroInvoke) {
          AWithStringMacroInvoke stringInvocation = (AWithStringMacroInvoke)invocation;

          // Create the argument map
          buffer.append("    {\n");
          buffer.append("    Map<String, Object> args = new HashMap<String, Object>();\n");
          
          // Push the arguments on
          for (PArgument pargument : stringInvocation.getArguments()) {
            AArgument argument = (AArgument)pargument;
            String expr = null;
            if (argument.getExpr() instanceof AStringExpr) {
              expr = "\"" + ((AStringExpr)argument.getExpr()).getString().getText() + "\"";
            } else if (argument.getExpr() instanceof APropertyExpr) {
              APropertyExpr propertyExpr = (APropertyExpr)argument.getExpr();
              if (propertyExpr.getName() instanceof ASimpleName) {
                ASimpleName simpleName = (ASimpleName)propertyExpr.getName();
                expr = "environment.get(\"" + simpleName.getIdentifier().getText() + "\")";
              }
            }
            buffer.append("    args.put(\"" + argument.getIdentifier().getText() + "\", " + expr + ");\n");
          }
          
          // Call the macro
          String macroName = stringInvocation.getIdentifier().getText();
          String macroValue = stringInvocation.getString().getText();
          buffer.append("    nodes.addAll(new macro" + macroName + "().invoke(args, \"" + macroValue + "\"));\n");
          buffer.append("    }\n");
        } else if (invocation instanceof AWithGeneratorMacroInvoke) {
          AWithGeneratorMacroInvoke generatorInvocation = (AWithGeneratorMacroInvoke)invocation;

          // Create the argument map
          buffer.append("    Map<String, Object> args = new HashMap<String, Object>();\n");
          
          // Push the arguments on
          for (PArgument pargument : generatorInvocation.getArguments()) {
            AArgument argument = (AArgument)pargument;
            String expr = null;
            if (argument.getExpr() instanceof AStringExpr) {
              expr = "\"" + ((AStringExpr)argument.getExpr()).getString().getText() + "\"";
            }
            buffer.append("    args.put(\"" + argument.getIdentifier().getText() + "\", " + expr + ");\n");
          }
          
          // Call the macro
          String macroName = generatorInvocation.getIdentifier().getText();
          int generatorId = generatorIds.get(generatorInvocation.getGenerator());
          buffer.append("    nodes.addAll(new macro" + macroName + "().invoke(args, new generator" + generatorId + "()));\n");
        }
        
      }
      
          
      @Override
      public void outANodeCreate(ANodeCreate node) {
        String id = node.getId().getText();

        // TODO Expression evaluation should be done way better!
        if (node.getExpr() instanceof AGeneratorExpr) {
          // Grab the named arguments to add to the node
          
          // Invoke the generator
          AGeneratorExpr generatorExp = (AGeneratorExpr)node.getExpr();
          String invokation = "new generator" + generatorIds.get(generatorExp.getGenerator()) + "().view(new HashMap<String, Object>())";
          buffers.peek().append("    {\n");
          buffers.peek().append("      Node n = new Node(" + invokation + ", \"" + id + "\", null);\n");

          addParameters(buffers.peek(), node);
          buffers.peek().append("      nodes.add(n);\n");
          
          buffers.peek().append("    }\n");
        } else if (node.getExpr() instanceof AStringExpr){
          AStringExpr stringExpr = (AStringExpr)node.getExpr();
          String text = stringExpr.getString().getText();
          buffers.peek().append("    {\n");
          buffers.peek().append("      Node n = new Node(null, \"" + id + "\", \"" + text + "\");\n");
          addParameters(buffers.peek(), node);
          buffers.peek().append("      nodes.add(n);\n");
          
          buffers.peek().append("    }\n");
        } else if (node.getExpr() instanceof APropertyExpr) {
          APropertyExpr identifierExpr = (APropertyExpr)node.getExpr();
          
          if (identifierExpr.getName() instanceof ASimpleName) {
            String text = ((ASimpleName)identifierExpr.getName()).getIdentifier().getText();
            buffers.peek().append("    {\n");
            
            // TODO Fix this minging hack - work out scopings etc
            buffers.peek().append("      Object v = args.get(\"" + text + "\");\n");
            buffers.peek().append("      if (v == null) {\n");
            buffers.peek().append("        v = environment.get(\"" + text + "\");\n");
            buffers.peek().append("      }\n");
            
            
            
            buffers.peek().append("      Node n = new Node(null, \"" + id + "\", v.toString());\n");
            addParameters(buffers.peek(), node);
            buffers.peek().append("      nodes.add(n);\n");
            
            buffers.peek().append("    }\n");
          } else {
            throw new RuntimeException("Evaluating properties on a controller not supported, yet");
          }          
        } else if (node.getExpr() instanceof AControllerPropExpr) {
          String text = nameToStringWithGets(((AControllerPropExpr)node.getExpr()).getName());
          buffers.peek().append("    {\n");
          buffers.peek().append("      Node n = new Node(null, \"" + id + "\", controller." + text + ");\n");
          addParameters(buffers.peek(), node);
          buffers.peek().append("      nodes.add(n);\n");
          
          buffers.peek().append("    }\n");
        }
      }

      private void addParameters(final StringBuffer buffer, ANodeCreate node) {
        // Add the parameters on
        for (PArgument _argument : node.getArguments()) {
          AArgument argument = (AArgument)_argument;

          String name = argument.getIdentifier().getText();
          String value = null;
          if (argument.getExpr() instanceof AStringExpr) {
            value = "\"" + ((AStringExpr)argument.getExpr()).getString().getText() + "\"";
          } else if (node.getExpr() instanceof APropertyExpr) {
            APropertyExpr identifierExpr = (APropertyExpr)argument.getExpr();
            
            if (identifierExpr.getName() instanceof ASimpleName) {
              String text = ((ASimpleName)identifierExpr.getName()).getIdentifier().getText();
              value = "environment.get(\"" + text + "\").toString()";
            } else {
              throw new RuntimeException("Evaluating properties on a controller not supported, yet");
            }          
          } else {
            throw new RuntimeException("Expressions not supported yet");
          }
          
          buffer.append("    n.setAttribute(\"" + name + "\", " + value + ");\n");
        }
      }
      
//      @Override
//      public void inAStringLiteral(AStringLiteral node) {
//        System.out.println(node.getString());
//        node.getString().setText(node.getString().getText().replaceAll("\"", ""));
//      }
    });
    
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
    
    System.out.println("WIN!");
  }

  public void compile(String source) {
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
    compile("basic-string", ast);
  }
}
