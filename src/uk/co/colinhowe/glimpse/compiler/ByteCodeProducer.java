/**
 * 
 */
package uk.co.colinhowe.glimpse.compiler;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicInteger;

import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import com.sun.tools.internal.xjc.generator.bean.MethodWriter;

import uk.co.colinhowe.glimpse.CompilationError;
import uk.co.colinhowe.glimpse.CompilationResult;
import uk.co.colinhowe.glimpse.Node;
import uk.co.colinhowe.glimpse.View;
import uk.co.colinhowe.glimpse.compiler.analysis.DepthFirstAdapter;
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
import uk.co.colinhowe.glimpse.compiler.node.AView;
import uk.co.colinhowe.glimpse.compiler.node.AWithGeneratorMacroInvoke;
import uk.co.colinhowe.glimpse.compiler.node.AWithInitVarDefn;
import uk.co.colinhowe.glimpse.compiler.node.AWithStringMacroInvoke;
import uk.co.colinhowe.glimpse.compiler.node.PArgDefn;
import uk.co.colinhowe.glimpse.compiler.node.PArgument;
import uk.co.colinhowe.glimpse.compiler.node.PExpr;
import uk.co.colinhowe.glimpse.compiler.node.PGenerator;
import uk.co.colinhowe.glimpse.compiler.node.PMacroInvoke;
import uk.co.colinhowe.glimpse.compiler.node.PName;
import uk.co.colinhowe.glimpse.compiler.node.PType;
import uk.co.colinhowe.glimpse.compiler.node.TString;
import uk.co.colinhowe.glimpse.compiler.typing.SimpleType;
import uk.co.colinhowe.glimpse.infrastructure.Scope;

public class ByteCodeProducer extends DepthFirstAdapter implements Opcodes {
  private final String nodeInitMethodSignature = "(Ljava/util/List;Ljava/lang/String;Ljava/lang/Object;)V";
  
  private final AtomicInteger generatorCount;
  private final List<String> methods;
  private final Stack<StringBuffer> buffers;
  private final Map<AGenerator, Integer> generatorIds;
  private final Set<String> currentMacroArguments;
  private final Set<String> macroDefns;
  private final List<String> macroFilenames;
  private final Stack<ClassVisitor> classVisitors;
  private final Stack<MethodVisitor> methodVisitors;
  private final LineNumberProvider lineNumberProvider;
  private final Stack<String> owners;
  private final String viewname;
  private final Stack<Label> labels;
  private final Stack<String> generatorNames;
  private final TypeProvider typeProvider;
  private final ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
  private final String outputFileName;
  private uk.co.colinhowe.glimpse.compiler.typing.Type controllerType = null;

  private final Stack<Scope> scopes;
  
  private Stack<ClassWriter> classWriters;
  
  private int methodsBeforeMacroDefinition = 0;

  public ByteCodeProducer(String viewname, AtomicInteger generatorCount, List<String> methods,
      Stack<StringBuffer> buffers, Map<AGenerator, Integer> generatorIds,
      Set<String> currentMacroArguments, Set<String> macroDefns,
      StringBuffer controllerType, List<String> macroFilenames, LineNumberProvider lineNumberProvider,
      final TypeProvider typeProvider, final String outputFileName) {
    this.generatorCount = generatorCount;
    this.methods = methods;
    this.buffers = buffers;
    this.generatorIds = generatorIds;
    this.currentMacroArguments = currentMacroArguments;
    this.macroDefns = macroDefns;
    this.macroFilenames = macroFilenames;
    this.lineNumberProvider = lineNumberProvider;
    this.typeProvider = typeProvider;
    
    this.scopes = new Stack<Scope>();
    this.classVisitors = new Stack<ClassVisitor>();
    this.methodVisitors = new Stack<MethodVisitor>();
    this.owners = new Stack<String>();
    this.viewname = viewname;
    this.labels = new Stack<Label>();
    this.classWriters = new Stack<ClassWriter>();
    this.generatorNames = new Stack<String>();
    this.outputFileName = outputFileName;
    
    classWriters.push(classWriter);
  }
  
  
  @Override
  public void outAView(AView node) {
    if (node.getStmt().size() > 0) {
      // Return the list of nodes
      MethodVisitor mv = methodVisitors.peek();
      Label l0 = labels.peek();
      
      Label l2 = new Label();
      mv.visitLabel(l2);
      mv.visitVarInsn(ALOAD, 2);
      mv.visitInsn(ARETURN);
      
      Label l3 = new Label();
      mv.visitLabel(l3);
      mv.visitLocalVariable("this", "Lcheese/HelloWorld;", null, l0, l3, 0);
      
      // TODO This isn't technically accurate... the start label is too early
      mv.visitLocalVariable("scope", "Luk/co/colinhowe/glimpse/infrastructure/Scope;", null, l0, l3, 1);
      mv.visitLocalVariable("nodes", "Ljava/util/List;", "Ljava/util/List<Luk/co/colinhowe/glimpse/Node;>;", l0, l3, 2);
      mv.visitLocalVariable("controller", "Ljava/lang/Object;", null, l0, l3, 3);
      mv.visitMaxs(0, 0);
      mv.visitEnd();
      
      classWriters.peek().visitEnd();
      
      scopes.pop();
      
      byte[] bytes = classWriter.toByteArray();
      
      // Output the class to a class file!
      File file = new File(outputFileName);
//      file.deleteOnExit();
        
      try {
        FileOutputStream stream = new FileOutputStream(file);
        stream.write(bytes);
        stream.close();
        
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    }
  }
  
  @Override
  public void outAPropertyExpr(APropertyExpr node) {
    MethodVisitor mv = methodVisitors.peek();
    
    // Get the value from the scope
    Label l1 = new Label();
    mv.visitLabel(l1);
    mv.visitVarInsn(ALOAD, 1); // scope
    
    if (node.getName() instanceof ASimpleName) {
      ASimpleName simpleName = (ASimpleName)node.getName();
      
      String name = simpleName.getIdentifier().getText();
      mv.visitLdcInsn(name); // name, scope
      debug("simpleExpr [" + simpleName.getIdentifier().getText() + "]");
    } else {
      throw new RuntimeException("Unsupported type of node");
    }
    
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;");

    
    // Leave the property on the stack for the next instruction to pick up

    // TODO only convert to a string at the top level expression
    // Probably use some cunning type conversion
    
    // TODO This is horrific. We should be type converting at last second.
    // Have an integer on the stack so convert it
//    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "toString", "()Ljava/lang/String;");
  }
  
  @Override
  public void outAControllerPropExpr(AControllerPropExpr node) {
    MethodVisitor mv = methodVisitors.peek();
    
    // Get the value from the scope
    Label l1 = new Label();
    mv.visitLabel(l1);
    mv.visitVarInsn(ALOAD, 1); // scope
    
    String name = "$controller";
    mv.visitLdcInsn(name); // name, scope
    debug("controller");
    
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;");
    mv.visitTypeInsn(CHECKCAST, getTypeName(controllerType));
    evaluateProperty(node.getName(), controllerType);
  }
  
  public void evaluateProperty(PName node, uk.co.colinhowe.glimpse.compiler.typing.Type ownerType) {
    final MethodVisitor mv = methodVisitors.peek();
    
    // The controller will be on the stack
    final String name;
    if (node instanceof ASimpleName) {
      final ASimpleName simpleName = (ASimpleName)node;
      name = simpleName.getIdentifier().getText();
    } else if (node instanceof AQualifiedName) {
      final AQualifiedName qualifiedName = (AQualifiedName)node;
      name = qualifiedName.getIdentifier().getText();
    } else {
      throw new RuntimeException("Type does not exist");
    }
    
    /// Determine the method return type
    String methodName = "get" + capitalise(name);
    String returnType;
    Class<?> returnClass;
    if (ownerType instanceof SimpleType) {
      try {
        returnClass = ((SimpleType)ownerType).getClazz().getMethod(methodName).getReturnType();
      } catch (Exception e) {
        throw new RuntimeException("Broken", e);
      }
      returnType = Type.getInternalName(returnClass);
    } else {
      throw new IllegalArgumentException("Only support simple types");
    }
    mv.visitMethodInsn(INVOKEVIRTUAL, getTypeName(ownerType), methodName, "()L" + returnType + ";");
    
    // Recurse if needed
    if (node instanceof AQualifiedName) {
      final AQualifiedName qualifiedName = (AQualifiedName)node;
      evaluateProperty(qualifiedName.getName(), new SimpleType(returnClass));
    }
  }
  
  private String capitalise(String s) {
    return s.substring(0, 1).toUpperCase() + s.substring(1);
  }
  
  private String getTypeName(uk.co.colinhowe.glimpse.compiler.typing.Type type) {
    if (type instanceof SimpleType) {
      return Type.getInternalName(((SimpleType)type).getClazz());
    } else {
      throw new IllegalArgumentException("Only support simple types");
    }
  }
  
  
  @Override
  public void inAView(AView node) {
    // Output a view class only if the view contains something that isn't a macro definition
    if (node.getStmt().size() > 0) {
      
      final ClassWriter classWriter = classWriters.peek();
      classWriter.visit(V1_6, ACC_PUBLIC + ACC_SUPER, viewname, null, "uk/co/colinhowe/glimpse/View", new String[] { });
  
      // TODO Move this next to initialisation if possible
      FieldVisitor fv = classWriter.visitField(
          ACC_PRIVATE, "globalScope", Type.getDescriptor(Scope.class), null, null);
      fv.visitEnd();
  
      owners.push(viewname);
      
      // Create a scope for the view
      final Scope scope = new Scope(null, false);
      scopes.add(scope);
      
      // TODO Handle components
  
      // Constructor
      {
        MethodVisitor mv = classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
  
        // Initialise super class
        Label l0 = new Label();
        mv.visitLabel(l0);
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL, "uk/co/colinhowe/glimpse/View", "<init>", "()V");
    
        // Initialise the scope
        Label l1 = new Label();
        mv.visitLabel(l1);
        mv.visitVarInsn(ALOAD, 0);
        mv.visitTypeInsn(NEW, Type.getInternalName(Scope.class));
        mv.visitInsn(DUP);
        mv.visitInsn(ACONST_NULL);
        mv.visitInsn(ICONST_0);
        mv.visitMethodInsn(INVOKESPECIAL, Type.getInternalName(Scope.class), "<init>", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Z)V"); 
        mv.visitFieldInsn(PUTFIELD, viewname, "globalScope", Type.getDescriptor(Scope.class));
        
        Label l2 = new Label();
        mv.visitLabel(l2);
        mv.visitInsn(RETURN);
    
        Label l3 = new Label();
        mv.visitLabel(l3);
        mv.visitLocalVariable("this", "L" + viewname + ";", null, l0, l3, 0);
        mv.visitMaxs(0, 0);
        mv.visitEnd();
      } 
      
      // Create the view method
      {
        // TODO Make this use the classes... currently mess!
        MethodVisitor mv = classWriter.visitMethod(
            ACC_PUBLIC, "view", "(Ljava/lang/Object;)Ljava/util/List;", "(Ljava/lang/Object;)Ljava/util/List<Luk/co/colinhowe/glimpse/Node;>;", null);
        mv.visitCode();
        
        Label l0 = new Label();
        labels.push(l0);
        mv.visitLabel(l0);
        mv.visitVarInsn(ALOAD, 0);
        
        // Put the controller in to register 3 so we don't have to rejig all the register accesses..
        mv.visitVarInsn(ALOAD, 1);
        mv.visitVarInsn(ASTORE, 3);
        mv.visitFieldInsn(GETFIELD, viewname, "globalScope", Type.getDescriptor(Scope.class));
        mv.visitVarInsn(ASTORE, 1);
        
        Label l1 = new Label();
        mv.visitLabel(l1);
        mv.visitTypeInsn(NEW, "java/util/LinkedList");
        mv.visitInsn(DUP);
        mv.visitMethodInsn(INVOKESPECIAL, "java/util/LinkedList", "<init>", "()V");
        mv.visitVarInsn(ASTORE, 2);
        
        // TODO Stuff goes here
        methodVisitors.push(mv);
      }
    }
  }
  
  
  public String nameToString(PName node) {
    // Chunk the name down
    PName nameNode = node;
    String name = "";
    
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
  
  public String getStringFromExpr(PExpr expr) {
    if (expr instanceof AConstantExpr) {
      return ((AConstantExpr)expr).getNumber().getText();
    }
    
    return "0";
  }
  

  @Override
  public void caseTString(TString node) {
    
    if (node.getText().startsWith("<")) {
      String[] lines = node.getText().split("\n");
      
      // Determine indentation from the last line - and then 2 spaces more
      // TODO Make this configurable... or nicer
      int indentation = lines[lines.length - 1].indexOf(">") + 2;
      
      // Remove the indentation from each line and build up a string to output
      final StringBuffer outputStringBuffer = new StringBuffer();
      for (int i = 1; i < lines.length - 1; i++) {
        outputStringBuffer.append(lines[i].substring(indentation) + "\n");
      }
      // Remove the trailing + and quote and new line
      final String text = outputStringBuffer.substring(0, outputStringBuffer.length() - 1);
      node.setText(text);
    } else {
      node.setText(node.getText().replaceAll("\"", ""));
    }
    
    // Put the text onto the stack
    MethodVisitor mv = methodVisitors.peek();
    mv.visitLdcInsn(node.getText());
    
    debug("string [" + node.getText() + "]");
  }
  
  private void debug(String debugText) {
    System.out.println("[debug:" + methodVisitors.peek() + "] " + debugText);
  }


  @Override
  public void outAController(AController node) {
    // The controller will be in register 3... put it on the environment
    MethodVisitor mv = methodVisitors.peek();
    mv.visitVarInsn(ALOAD, 1);
    mv.visitLdcInsn("$controller");
    mv.visitVarInsn(ALOAD, 3);
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V");
    
    String className = node.getName().toString().trim().replaceAll(" ", ".");
    debug("controller has type [" + className + "]");
    
    // Load the controller class
    try {
      Class<?> clazz = this.getClass().getClassLoader().loadClass(className);
      controllerType = new SimpleType(clazz);
    } catch (ClassNotFoundException e) {
      throw new RuntimeException("Controller class [" + className + "] not found");
    }
  }

  
  @Override
  public void inAMacroDefn(AMacroDefn node) {
    
    // Create a scope for this macro
    final Scope scope = new Scope(
        scopes.empty() ? null : scopes.peek(), true);
    scopes.add(scope);
    
    // TODO Macros really should be ripped out into their own ASTs and processed separately
    methodsBeforeMacroDefinition = methods.size();
    
    // Create arguments 
    for (PArgDefn pargDefn : node.getArgDefn()) {
      if (pargDefn instanceof AArgDefn) {
        AArgDefn argDefn = (AArgDefn)pargDefn;
        currentMacroArguments.add(argDefn.getIdentifier().getText());
      }
    }
    
    // Create a new top level class
    final ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classWriters.push(cw);
    
    // Get the name of the macro
    final String macroName = node.getName().getText();
    
    cw.visit(V1_6, ACC_SUPER, macroName, null, "java/lang/Object", null);
    
    // Constructor
    {
      MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
      mv.visitCode();

      // Initialise super class
      Label l0 = new Label();
      mv.visitLabel(l0);
      mv.visitVarInsn(ALOAD, 0);
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
      mv.visitInsn(RETURN);
  
      Label l3 = new Label();
      mv.visitLabel(l3);
      mv.visitLocalVariable("this", "L" + macroName + ";", null, l0, l3, 0);
      mv.visitMaxs(0, 0);
      mv.visitEnd();
    } 
    
    
    // Invoke method
    {
      String contentType = null;
      if (node.getContentType() instanceof AStringType) {
        contentType = "java/lang/String";
      } else {
        contentType = "uk/co/colinhowe/glimpse/Generator";
      }
      String valueName = node.getContentName().getText();
      
      MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, 
          "invoke", 
          "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Ljava/util/Map;L" + contentType + ";)Ljava/util/List;",
          "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Ljava/util/Map<Ljava/lang/String;Ljava/lang/Object;>;L" + contentType + ";)Ljava/util/List<Luk/co/colinhowe/glimpse/Node;>;", null);
      methodVisitors.push(mv);
      mv.visitCode();

      // TODO This or something similar could be in a parent class...
      
      // Variables
      // 0 -> this
      // 1 -> caller scope
      // 2 -> argument map
      // 3 -> content
      // 4 -> macro scope
      

      // Create a scope for the macro
      Label l0 = new Label();
      mv.visitLabel(l0);
      mv.visitTypeInsn(NEW, "uk/co/colinhowe/glimpse/infrastructure/Scope");
      mv.visitInsn(DUP);
      mv.visitVarInsn(ALOAD, 1);
      mv.visitInsn(ICONST_1);
      mv.visitMethodInsn(INVOKESPECIAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "<init>", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Z)V");
      mv.visitVarInsn(ASTORE, 4);
      
      Label l1 = new Label();
      mv.visitLabel(l1);
      mv.visitVarInsn(ALOAD, 2);
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Map", "entrySet", "()Ljava/util/Set;");
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Set", "iterator", "()Ljava/util/Iterator;");
      mv.visitVarInsn(ASTORE, 6);
      
      Label l2 = new Label();
      mv.visitJumpInsn(GOTO, l2);
      
      Label l3 = new Label();
      mv.visitLabel(l3);
      mv.visitFrame(Opcodes.F_FULL, 7, 
          new Object[] { macroName, "uk/co/colinhowe/glimpse/infrastructure/Scope", "java/util/Map", contentType, "uk/co/colinhowe/glimpse/infrastructure/Scope", Opcodes.TOP, "java/util/Iterator"}, 0, new Object[] {});
      mv.visitVarInsn(ALOAD, 6);
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Iterator", "next", "()Ljava/lang/Object;");
      mv.visitTypeInsn(CHECKCAST, "java/util/Map$Entry");
      mv.visitVarInsn(ASTORE, 5);
      
      Label l4 = new Label();
      mv.visitLabel(l4);
      mv.visitVarInsn(ALOAD, 4);
      mv.visitVarInsn(ALOAD, 5);
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Map$Entry", "getKey", "()Ljava/lang/Object;");
      mv.visitTypeInsn(CHECKCAST, "java/lang/String");
      mv.visitVarInsn(ALOAD, 5);
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Map$Entry", "getValue", "()Ljava/lang/Object;");
      mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V");

      mv.visitLabel(l2);
      mv.visitFrame(Opcodes.F_SAME, 0, null, 0, null);
      mv.visitVarInsn(ALOAD, 6);
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Iterator", "hasNext", "()Z");
      mv.visitJumpInsn(IFNE, l3);
      
      // Put the content onto the scope
      mv.visitVarInsn(ALOAD, 4); // scope
      mv.visitLdcInsn(valueName); // name, scope
      mv.visitVarInsn(ALOAD, 3); // value, name, scope
      mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V");
      
      Label l6 = new Label();
      mv.visitLabel(l6);
      mv.visitLocalVariable("this", "Lcheese/HelloWorld;", null, l0, l6, 0);
      mv.visitLocalVariable("callerScope", "Luk/co/colinhowe/glimpse/infrastructure/Scope;", null, l0, l6, 1);
      mv.visitLocalVariable("_args", "Ljava/util/Map;", "Ljava/util/Map<Ljava/lang/String;Ljava/lang/Object;>;", l0, l6, 2);
      mv.visitLocalVariable("value", "Ljava/lang/String;", null, l0, l6, 3);
      mv.visitLocalVariable("scope", "Luk/co/colinhowe/glimpse/infrastructure/Scope;", null, l1, l6, 4);
      mv.visitLocalVariable("entry", "Ljava/util/Map$Entry;", "Ljava/util/Map$Entry<Ljava/lang/String;Ljava/lang/Object;>;", l4, l2, 5);
    }
    
  }
  
  
  @Override
  public void outAMacroDefn(AMacroDefn node) {
    // Remove the scope
    scopes.pop();
    
    final MethodVisitor mv = methodVisitors.pop();
    
    {
      // The generator will be on the stack
      int generatorId = generatorIds.get(node.getGenerator());
      String generatorIdentifier = viewname + "$$generator" + generatorId;

      // generator
      mv.visitVarInsn(ALOAD, 4); // scope, generator
      mv.visitMethodInsn(INVOKEVIRTUAL, generatorIdentifier, "view", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;)Ljava/util/List;");
      // nodes
  
      mv.visitInsn(ARETURN);
  
      mv.visitMaxs(4, 7);
      mv.visitEnd();
    }
    
    final ClassWriter cw = classWriters.pop();

    // Get the name of the macro
    final String macroName = node.getName().getText();


    cw.visitEnd();
    
    // Write the bytes out
    byte[] bytes = cw.toByteArray();
    
    // Output the class to a class file!
    File file = new File("temp/" + macroName + ".class");
//    file.deleteOnExit();
      
    try {
      FileOutputStream stream = new FileOutputStream(file);
      stream.write(bytes);
      stream.close();
      
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    
    // Bin all the arguments
    currentMacroArguments.clear();
  }
  
  @Override
  public void outAArgument(AArgument node) {
  }

  @Override
  public void outAIncludeStmt(AIncludeStmt node) {
    MethodVisitor mv = methodVisitors.peek();
    
    AIncludeA include = (AIncludeA)node.getIncludeA();

    // Create a new scope for the generator
    mv.visitTypeInsn(NEW, Type.getInternalName(Scope.class));
    mv.visitInsn(DUP);
    mv.visitVarInsn(ALOAD, 1);
    mv.visitInsn(ICONST_0);
    mv.visitMethodInsn(INVOKESPECIAL, Type.getInternalName(Scope.class), "<init>", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Z)V"); 
    
    // Populate the scope with any arguments
    // The arguments will be on the stack like so: scope, name, value, name, value

    // For each argument we need to transform the stack to be like:
    //   value, name, scope, scope, ...
    
    // Starts as scope, name, value, name, value
    // Process the arguments in reverse order as the values
    // will have been added on to the stack in forward order
    List<PArgument> reverseArguments = new LinkedList<PArgument>();
    for (PArgument parg : include.getArguments()) {
      reverseArguments.add(0, parg);
    }
    
    for (PArgument pargument : reverseArguments) {
      // TODO Check types
      mv.visitInsn(DUP_X1); // scope, value, scope, ...
      mv.visitInsn(DUP_X1); // scope, value, scope, scope, ...
      mv.visitInsn(POP); // value, scope, scope, ...
      
      AArgument arg = (AArgument)pargument;
      mv.visitLdcInsn(arg.getIdentifier().getText());
      // name, value, scope, scope, ...
      mv.visitInsn(SWAP); // value, name, scope, scope, ...
      // Up cast to an integer
      // TODO Proper typing
//      mv.visitTypeInsn(NEW, "java/lang/Integer");
//      mv.visitInsn(DUP_X1); 
//      mv.visitInsn(SWAP);
//      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V");
//      final uk.co.colinhowe.glimpse.compiler.typing.Type type;
//      type = (uk.co.colinhowe.glimpse.compiler.typing.Type)scopes.peek().get(simpleName.getIdentifier().getText());

      mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V");
    }
    // Ends with the scope at the top of the stack

    String generatorArgumentName = include.getTheInclude().getText();
    
    // Pull the generator from the scope
    Label l1 = new Label();
    mv.visitLabel(l1);
    mv.visitVarInsn(ALOAD, 1); // scope
    mv.visitLdcInsn(generatorArgumentName); // name, scope
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;");
    
    // The generator will be on the stack
    mv.visitTypeInsn(CHECKCAST, "uk/co/colinhowe/glimpse/Generator");

    // invoke the generator
    mv.visitInsn(SWAP);
    mv.visitMethodInsn(INVOKEINTERFACE, "uk/co/colinhowe/glimpse/Generator", "view", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;)Ljava/util/List;");

    // Nodes now sit on the stack
    mv.visitVarInsn(ALOAD, 2); // list, nodes
    mv.visitInsn(SWAP);
    mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "addAll", "(Ljava/util/Collection;)Z");
    mv.visitInsn(POP);
  }

  @Override
  public void inAGenerator(AGenerator node) {
    
    // Create a scope for this generator
    final Scope scope = new Scope(scopes.peek(), false);
    scopes.add(scope);
    
    final ClassWriter outerClassWriter = classWriters.peek();
    
    // Get the ID for the generator
    final int id = generatorCount.getAndIncrement();
    final String generatorName = "$generator" + id;

    generatorNames.push(generatorName);
    
    outerClassWriter.visitInnerClass(viewname + "$" + generatorName, viewname, generatorName, ACC_PRIVATE + ACC_STATIC);

    final ClassWriter innerClassWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    classWriters.push(innerClassWriter);

    innerClassWriter.visit(V1_6, ACC_SUPER, viewname + "$" + generatorName, null, "java/lang/Object", new String[] { "uk/co/colinhowe/glimpse/Generator" });
    innerClassWriter.visitInnerClass(viewname + "$" + generatorName, viewname, generatorName, ACC_PRIVATE + ACC_STATIC);

    generatorIds.put(node, id);

    owners.push(viewname);
    
    // Constructor
    {
      MethodVisitor mv = innerClassWriter.visitMethod(ACC_PRIVATE, "<init>", "()V", null, null);
      mv.visitCode();

      // Initialise super class
      Label l0 = new Label();
      mv.visitLabel(l0);
      mv.visitVarInsn(ALOAD, 0);
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
      mv.visitInsn(RETURN);
  
      Label l3 = new Label();
      mv.visitLabel(l3);
      mv.visitLocalVariable("this", "L" + viewname + "$" + generatorName + ";", null, l0, l3, 0);
      mv.visitMaxs(0, 0);
      mv.visitEnd();
    } 
    
    // Inner constructor
    {
      MethodVisitor mv = innerClassWriter.visitMethod(ACC_SYNTHETIC, "<init>", "(L" + viewname + "$" + generatorName + ";)V", null, null);
      mv.visitCode();
      Label l0 = new Label();
      mv.visitLabel(l0);
      mv.visitLineNumber(34, l0);
      mv.visitVarInsn(ALOAD, 0);
      mv.visitMethodInsn(INVOKESPECIAL, viewname + "$" + generatorName, "<init>", "()V");
      mv.visitInsn(RETURN);
      mv.visitMaxs(0, 0);
      mv.visitEnd();
    }
    
    // Create the view method
    {
      // TODO Make this use the classes... currently mess!
      MethodVisitor mv = innerClassWriter.visitMethod(
          ACC_PUBLIC, "view", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;)Ljava/util/List;", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;)Ljava/util/List<Luk/co/colinhowe/glimpse/Node;>;", null);
      mv.visitCode();
      
      Label l1 = new Label();
      mv.visitLabel(l1);
      mv.visitTypeInsn(NEW, "java/util/LinkedList");
      mv.visitInsn(DUP);
      mv.visitMethodInsn(INVOKESPECIAL, "java/util/LinkedList", "<init>", "()V");
      mv.visitVarInsn(ASTORE, 2);
      
      // TODO Stuff goes here
      methodVisitors.push(mv);
      labels.push(l1);
    }
  }

  @Override
  public void outAGenerator(AGenerator node) {
    scopes.pop();
    
    // Return the list of nodes
    MethodVisitor mv = methodVisitors.pop();
    Label l0 = labels.pop();
    
    Label l2 = new Label();
    mv.visitLabel(l2);
    mv.visitVarInsn(ALOAD, 2);
    mv.visitInsn(ARETURN);
    
    Label l3 = new Label();
    mv.visitLabel(l3);
    
    String generatorName = generatorNames.pop();
    
    mv.visitLocalVariable("this", "L" + viewname + "$" + generatorName + ";", null, l0, l3, 0);
    
    // TODO This isn't technically accurate... the start label is too early
    mv.visitLocalVariable("scope", "Luk/co/colinhowe/glimpse/infrastructure/Scope;", null, l0, l3, 1);
    mv.visitLocalVariable("nodes", "Ljava/util/List;", "Ljava/util/List<Luk/co/colinhowe/glimpse/Node;>;", l0, l3, 2);
    mv.visitMaxs(0, 0);
    mv.visitEnd();
    
    ClassWriter cw = classWriters.pop();
    cw.visitEnd();
    
    // Write the bytes out
    byte[] bytes = cw.toByteArray();
    
    // Output the class to a class file!
    File file = new File("temp/" + viewname + "$" + generatorName + ".class");
//    file.deleteOnExit();
      
    try {
      FileOutputStream stream = new FileOutputStream(file);
      stream.write(bytes);
      stream.close();
      
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    
    
    // Create an instance of the generator
    String generatorIdentifier = viewname + "$" + generatorName;
    mv = methodVisitors.peek();
    mv.visitTypeInsn(NEW, generatorIdentifier);
    mv.visitInsn(DUP); // generator, generator, args
    mv.visitInsn(ACONST_NULL); // null, generator, generator, args
    mv.visitMethodInsn(INVOKESPECIAL, generatorIdentifier, "<init>", "(L" + generatorIdentifier + ";)V");
    // generator, args    
    
    debug("generator [" + generatorIdentifier + "]");
  }

  @Override
  public void outAWithInitVarDefn(AWithInitVarDefn node) {
    MethodVisitor mv = methodVisitors.peek();

    final String varname = node.getIdentifier().getText();
    Label l1 = new Label();
    mv.visitLabel(l1);
    mv.visitVarInsn(ALOAD, 1);
    mv.visitLdcInsn(varname);

    PType type = node.getType();
    String typeString = "";
    if (type instanceof AIntType) {
//      mv.visitLocalVariable("x", "I", null, l1, l2, 2);
      int i = Integer.parseInt(getStringFromExpr(node.getExpr()));
      mv.visitIntInsn(SIPUSH, i);
      mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;");
    } else if (type instanceof AStringType) {
      typeString = "String";
    } else if (type instanceof AGeneratorType) {
      throw new RuntimeException("Cannot declare a generator as a variable");
    }

    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "add", "(Ljava/lang/String;Ljava/lang/Object;)V");
    
    // Put this variable and the type of it on to the scope
    scopes.peek().add(varname, typeProvider.getType(node));
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
    
    // Put this variable and the type of it on to the scope
    scopes.peek().add(varname, typeProvider.getType(node));
  }

  @Override
  public void outAIncrementStmt(AIncrementStmt node) {
    
    // Get the value from the scope
    final MethodVisitor mv = methodVisitors.peek();
    mv.visitVarInsn(ALOAD, 1); //scope
    mv.visitInsn(DUP); // scope, scope
    mv.visitLdcInsn(node.getIdentifier().getText()); // name, scope, scope
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "get", "(Ljava/lang/String;)Ljava/lang/Object;");
    // value, scope
    mv.visitTypeInsn(CHECKCAST, "java/lang/Integer");
    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I");
    mv.visitInsn(ICONST_1); // 1, value, scope
    mv.visitInsn(IADD); // value+1, scope
    mv.visitLdcInsn(node.getIdentifier().getText()); // name, value+1, scope
    mv.visitInsn(SWAP); // value+1, name, scope
    mv.visitTypeInsn(NEW, "java/lang/Integer"); // i, value+1, name, scope
    mv.visitInsn(DUP_X1); // i, value+1, i, name, scope
    mv.visitInsn(SWAP); // value+1, i, i, name, scope
    mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V");
    // i, name, scope

    mv.visitTypeInsn(CHECKCAST, "java/lang/Integer");
    
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "replace", "(Ljava/lang/String;Ljava/lang/Object;)V");
  }

  @Override
  public void outAAssignmentStmt(AAssignmentStmt node) {
    final MethodVisitor mv = methodVisitors.peek();
    
    // The value will be sitting on the stack
    mv.visitVarInsn(ALOAD, 1); // scope, value
    mv.visitInsn(SWAP); // value, scope

    mv.visitLdcInsn(node.getIdentifier().getText()); // name, scope, value
    mv.visitInsn(SWAP); // value, name, scope
    mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/infrastructure/Scope", "replace", "(Ljava/lang/String;Ljava/lang/Object;)V");
  }
  
  @Override
  public void outAConstantExpr(AConstantExpr node) {
    MethodVisitor mv = methodVisitors.peek();
    int i = Integer.valueOf(node.getNumber().getText());
    mv.visitLdcInsn(i);

    // Up-cast to an Integer
    // we don't like leave primitive types on the stack
    // This is inefficient but it simplifies implementation
    mv.visitTypeInsn(NEW, "java/lang/Integer");
    mv.visitInsn(DUP_X1); 
    mv.visitInsn(SWAP);
    mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V");
    
    debug("constant [" + i + "]");
  }

  @Override
  public void outAMacroStmt(AMacroStmt node) {
    StringBuffer buffer = buffers.peek();
    PMacroInvoke invocation = node.getMacroInvoke();
    MethodVisitor mv = methodVisitors.peek();
    
    // The arguments will be on the stack already.
    // The stack will look like:
    //   macro value, arg, arg, ...
    
    // Put all the arguments into a map
    mv.visitTypeInsn(NEW, "java/util/HashMap"); // args, macro value, arg
    mv.visitInsn(DUP); // args, args, macro value, arg
    mv.visitMethodInsn(INVOKESPECIAL, "java/util/HashMap", "<init>", "()V");
    // args, macro value, arg
    mv.visitTypeInsn(CHECKCAST, "java/util/Map"); // args, macro value, arg

    final List<PArgument> args;
    if (invocation instanceof AWithStringMacroInvoke) {
      args = ((AWithStringMacroInvoke)invocation).getArguments();
    } else if (invocation instanceof AWithGeneratorMacroInvoke) {
      args = ((AWithGeneratorMacroInvoke)invocation).getArguments();
    } else {
      throw new RuntimeException("Unsupported");
    }
    
    for (PArgument pargument : args) {
      AArgument argument = (AArgument)pargument;

      // Shuffle the stack around so the macro value and args are at the bottom
      // TODO must be a neater way to do this! :)
      // args, macro value, arg
      mv.visitInsn(DUP2_X1); // args, macro value, arg, args, macro value
      mv.visitInsn(POP); // macro value, arg, args, macro value
      mv.visitInsn(POP); // arg, args, macro value
      mv.visitInsn(SWAP); // args, arg, macro value
      mv.visitInsn(DUP_X2); // args, arg, macro value, args
      mv.visitInsn(SWAP); // arg, args, macro value, args
      
      // Put the variable name on
      mv.visitLdcInsn(argument.getIdentifier().getText());
      // name, arg, args, macro value, args
      mv.visitInsn(SWAP); // arg, name, args, macro value, args
      
      // Put the variable on the arguments
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Map", "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
      // object, macro value, args
      mv.visitInsn(POP); // macro value, args
      mv.visitInsn(SWAP); // args, macro value
    }
    
    if (invocation instanceof AWithStringMacroInvoke) {
      // Stack: args, value (string)
      mv.visitInsn(SWAP); // value, args
      mv.visitTypeInsn(CHECKCAST, "java/lang/String");

      AWithStringMacroInvoke stringInvocation = (AWithStringMacroInvoke)invocation;
      String macroName = stringInvocation.getIdentifier().getText();

      mv.visitTypeInsn(NEW, macroName); // macro, value, args
      mv.visitInsn(DUP); // macro, macro, value, args
      mv.visitMethodInsn(INVOKESPECIAL, macroName, "<init>", "()V");
      // macro, value, args
      mv.visitInsn(DUP_X2); // macro, value, args, macro
      mv.visitInsn(POP); // value, args, macro

      mv.visitVarInsn(ALOAD, 1); // scope, value, args, macro
      mv.visitInsn(DUP_X2); // scope, value, args, scope, macro
      mv.visitInsn(POP); // value, args, scope, macro

      mv.visitMethodInsn(INVOKEVIRTUAL, macroName, "invoke", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Ljava/util/Map;Ljava/lang/String;)Ljava/util/List;");
      // nodes
      mv.visitVarInsn(ALOAD, 2); // list, nodes
      mv.visitInsn(SWAP); // nodes, list
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "addAll", "(Ljava/util/Collection;)Z");
      mv.visitInsn(POP);

    } else if (invocation instanceof AWithGeneratorMacroInvoke) {
      // Stack: args, generator

      AWithGeneratorMacroInvoke generatorInvocation = (AWithGeneratorMacroInvoke)invocation;
      String macroName = generatorInvocation.getIdentifier().getText();

      mv.visitInsn(SWAP); // generator, args
      mv.visitTypeInsn(NEW, macroName); // macro, generator, args
      mv.visitInsn(DUP); // macro, macro, generator, args
      mv.visitMethodInsn(INVOKESPECIAL, macroName, "<init>", "()V");
      // macro, generator, args
      mv.visitInsn(DUP_X2); // macro, generator, args, macro
      mv.visitInsn(POP); // generator, args, macro

      mv.visitVarInsn(ALOAD, 1); // scope, generator, args, macro
      mv.visitInsn(DUP_X2); // scope, generator, args, scope, macro
      mv.visitInsn(POP); // generator, args, scope, macro
      
      // -> generator, args, scope, macro
      mv.visitMethodInsn(INVOKEVIRTUAL, macroName, "invoke", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;Ljava/util/Map;Luk/co/colinhowe/glimpse/Generator;)Ljava/util/List;");

      // nodes
      mv.visitVarInsn(ALOAD, 2); // list, nodes
      mv.visitInsn(SWAP); // list, nodes
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "addAll", "(Ljava/util/Collection;)Z");
      mv.visitInsn(POP);
    }
  }
  
  
  /**
   * When we come in to this method we expect the following stack:
   *   value
   *   property value -- Property pairs
   *   property name  -|
   */
  @Override
  public void outANodeCreate(ANodeCreate node) {
    String id = node.getId().getText();
    MethodVisitor mv = methodVisitors.peek();
    
    // Start creating the node
    // Load up the node list ready for adding the node to
    // Create the node on the stack ready for setting properties on it
    Label l1 = new Label();
    mv.visitLabel(l1);
//    mv.visitVarInsn(ALOAD, 2); // list, value
    
  mv.visitTypeInsn(NEW, "uk/co/colinhowe/glimpse/Node"); // node, value
  mv.visitInsn(DUP_X1); // node, value, node
  mv.visitInsn(SWAP); // value, node, node
    
    // TODO Expression evaluation should be done way better!
    if (node.getExpr() instanceof AGeneratorExpr) {
      // TODO Create the generator object :)
      AGeneratorExpr generatorExp = (AGeneratorExpr)node.getExpr();

      String generatorIdentifier = viewname + "$$generator" + generatorIds.get(generatorExp.getGenerator());

      // Stack: generator, node, node
      mv.visitVarInsn(ALOAD, 1); // scope, generator, node, node
      mv.visitMethodInsn(INVOKEVIRTUAL, generatorIdentifier, "view", "(Luk/co/colinhowe/glimpse/infrastructure/Scope;)Ljava/util/List;");
      // nodes, node, node
      mv.visitLdcInsn(id);  // id, nodes, node, node
      mv.visitInsn(ACONST_NULL); // null, id, nodes, node, node
      mv.visitMethodInsn(INVOKESPECIAL, "uk/co/colinhowe/glimpse/Node", "<init>", nodeInitMethodSignature);
      // node

      // Set properties on the node
      addParameters(mv, node);
      
      mv.visitVarInsn(ALOAD, 2); // list, node
      mv.visitInsn(SWAP); // node, list
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z");
      mv.visitInsn(POP);
    } else {
      
      /*
       * Output bytecode equivalent to:
       *   Node n = new Node(null, "\"" + id + "\", \"" + text + "\"");
       *   nodes.add(n);
       */
      
      // TODO Store the node off in a local variable, n
      // Then visitLocalVariable in this method.
      // This is fine as we can limit scope of the variable using labels and all is good
      
      // The value is on the top of the stack
      Label l2 = new Label();
      mv.visitLabel(l2);
//      mv.visitTypeInsn(NEW, "uk/co/colinhowe/glimpse/Node");
//      mv.visitInsn(DUP_X1); // node, value, node
//      mv.visitInsn(SWAP); // value, node, node
      mv.visitInsn(ACONST_NULL); // null, value, node, node
      mv.visitInsn(SWAP); // value, null, node, node
      mv.visitLdcInsn(id); // id, value, null, node, node
      mv.visitInsn(SWAP); // value, id, null, node, node
      mv.visitMethodInsn(INVOKESPECIAL, "uk/co/colinhowe/glimpse/Node", "<init>", nodeInitMethodSignature);
      // node
      
      // Set properties on the node
      addParameters(mv, node);
      
      // node
      
      mv.visitVarInsn(ALOAD, 2); // list, node
      mv.visitInsn(SWAP); // node, list
      mv.visitMethodInsn(INVOKEINTERFACE, "java/util/List", "add", "(Ljava/lang/Object;)Z");
      // boolean
      mv.visitInsn(POP);
    }
  }
  
  
  /**
   * Assume that the node is already on the stack and that the node must remain
   * on the stack afterwards.
   * 
   * @param mw
   * @param node
   */
  private void addParameters(final MethodVisitor mv, ANodeCreate node) {
    // Stack starts like this:
    // node, values...

    // Add the parameters on
    // In reverse order as the expressions are added on to 
    // the call in order which inverts them (due to it being a stack)
    List<PArgument> reverseArguments = new LinkedList<PArgument>();
    for (PArgument arg : node.getArguments()) {
      reverseArguments.add(0, arg);
    }
    for (PArgument _argument : reverseArguments) {
      AArgument argument = (AArgument)_argument;

      String name = argument.getIdentifier().getText();
      
      // node, value
      mv.visitInsn(DUP_X1); // node, value, node
      mv.visitInsn(SWAP); // value, node, node
      mv.visitLdcInsn(name); // name, value, node, node
      mv.visitInsn(SWAP); // value, name, node, node
      mv.visitMethodInsn(INVOKEVIRTUAL, "uk/co/colinhowe/glimpse/Node", "setAttribute", "(Ljava/lang/String;Ljava/lang/Object;)V");
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
          value = "scope.get(\"" + text + "\").toString()";
        } else {
          throw new RuntimeException("Evaluating properties on a controller not supported, yet");
        }          
      } else {
        throw new RuntimeException("Expressions not supported yet");
      }
      
      buffer.append("    n.setAttribute(\"" + name + "\", " + value + ");\n");
    }
  }
}