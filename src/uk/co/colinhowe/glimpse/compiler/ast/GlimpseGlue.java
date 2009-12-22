package uk.co.colinhowe.glimpse.compiler.ast;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;

import javax.management.RuntimeErrorException;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.tree.Tree;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import uk.co.colinhowe.glimpse.compiler.ast.GlimpseParser.V;

public class GlimpseGlue {

  public static void ass() {
    
  }
  
  public static class ClassGenerator implements Opcodes {
    private ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    private FieldVisitor fv;
    private MethodVisitor mv;
    private AnnotationVisitor av0;
    
    private void createViewClass(Tree tree) {
      cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, "uk/co/colinhowe/glimpse/example/View", null, "java/lang/Object", null);
      cw.visitSource("HelloWorld.java", null);

      {
        mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
        Label l0 = new Label();
        mv.visitLabel(l0);
        mv.visitLineNumber(8, l0);
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
        mv.visitInsn(RETURN);
        Label l1 = new Label();
        mv.visitLabel(l1);
        mv.visitLocalVariable("this", "Luk/co/colinhowe/glimpse/example/View;", null, l0, l1, 0);
        mv.visitMaxs(1, 1);
        mv.visitEnd();
      }
      
      // Create the main function
      mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "generateNodeTree", "()Ljava/util/List;", "()Ljava/util/List<Ljava/lang/Object;>;", null);
      mv.visitCode();

      // new LinkedList<Integer>
/*      {
      Label l0 = new Label();
      mv.visitLabel(l0);
      mv.visitLineNumber(11, l0);
      mv.visitTypeInsn(NEW, "java/util/LinkedList");
      mv.visitInsn(DUP);
      mv.visitMethodInsn(INVOKESPECIAL, "java/util/LinkedList", "<init>", "()V");
      mv.visitVarInsn(ASTORE, 0);
      
      // Return
      Label l1 = new Label();
      mv.visitLabel(l1);
      mv.visitLineNumber(12, l1);
      mv.visitVarInsn(ALOAD, 0);
      mv.visitInsn(ARETURN);
      
      // Creation of local variable ints - bit useless
      Label l2 = new Label();
      mv.visitLabel(l2);
      mv.visitLocalVariable("ints", "Ljava/util/List;", "Ljava/util/List<Ljava/lang/Integer;>;", l1, l2, 0);
      mv.visitMaxs(2, 1);
      mv.visitEnd();
      }*/
      
      // Create nodes local variable
      {
        Label l0 = new Label();
        mv.visitLabel(l0);
        mv.visitLineNumber(11, l0);
        mv.visitTypeInsn(NEW, "java/util/LinkedList");
        mv.visitInsn(DUP);
        mv.visitMethodInsn(INVOKESPECIAL, "java/util/LinkedList", "<init>", "()V");
        mv.visitVarInsn(ASTORE, 0);
        
        // Create calls for each node
        processTree(tree);
        
        // Return the nodes
        Label l1 = new Label();
        mv.visitLabel(l1);
        mv.visitLineNumber(12, l1);
        mv.visitVarInsn(ALOAD, 0);
        mv.visitInsn(ARETURN);
        
        // Creation of the nodes variable - done at the end when we know the instructions that use it
        Label l2 = new Label();
        mv.visitLabel(l2);
        mv.visitLocalVariable("nodes", "Ljava/util/List;", "Ljava/util/List<Ljava/lang/Integer;>;", l1, l2, 0);
        mv.visitMaxs(0, 0);
        mv.visitEnd();
      }
      
      cw.visitEnd();
      
      // Output the class
      final byte[] bytes= cw.toByteArray();
      FileOutputStream output = null;
      try {
        output = new FileOutputStream("generated-classes/uk/co/colinhowe/glimpse/example/View.class");
        output.write(bytes);
      } catch (Exception e) {
        throw new RuntimeException(e);
      } finally {
        if (output != null) {
          try {
            output.close();
          } catch (IOException e) {
            throw new RuntimeException(e);
          }
        }
      }
    }

    private void processTree(Tree tree) {
      for (int i = 0; i < tree.getChildCount(); i++) {
        Tree child = tree.getChild(i);
        
        if (child instanceof V) {
          node((V)child);
        } else {
          node(child);
        }
      }
    }
    
    private void node(Tree t) {
      System.out.println("TREE");
    }
    
    private void node(V t) {
      
      Label l0 = new Label();
      mv.visitLabel(l0);
      mv.visitLineNumber(17, l0);
      mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
      mv.visitLdcInsn(t.id);
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");

//      Label l2 = new Label();
//      mv.visitLabel(l2);
//      mv.visitLineNumber(20, l2);
//      mv.visitInsn(RETURN);
//
//      Label l3 = new Label();
//      mv.visitLabel(l3);
//      mv.visitLocalVariable("args", "[Ljava/lang/String;", null, l0, l3, 0);
    }
  }
  
  public static void main(String[] args) {
   try {
       CharStream input = new ANTLRStringStream("node:a\nnode:b\nnode:c");
       GlimpseLexer lex = new GlimpseLexer(input);

       CommonTokenStream tokens = new CommonTokenStream(lex);
       GlimpseParser parser = new GlimpseParser(tokens);
       GlimpseParser.view_return root = parser.view();
       System.out.println("tree="+((Tree)root.tree).toStringTree());
       
       new ClassGenerator().createViewClass((Tree)root.tree);
       
       // Run the class
       runClass("G:/workspace/grammar-sandbox/generated-classes/uk/co/colinhowe/glimpse/example/View");

       
   } catch(Throwable t) {
       System.out.println("exception: "+t);
       t.printStackTrace();
   }
  }
  
  
  @SuppressWarnings("deprecation")
  private static void runClass(String fileName) {
    // Create a File object on the root of the directory containing the class file
    File file = new File(fileName);
    
    try {
        // Convert File to a URL
        URL url = file.toURL();          // file:/c:/myclasses/
        URL[] urls = new URL[]{url};
    
        // Create a new class loader with the directory
        ClassLoader cl = new URLClassLoader(urls);
    
        // Load in the class; MyClass.class should be located in
        // the directory file:/c:/myclasses/com/mycompany
        Class<?> cls = cl.loadClass("uk.co.colinhowe.glimpse.example.View");
        
        cls.getMethod("generateNodeTree").invoke(null);
    } catch (MalformedURLException e) {
      e.printStackTrace();
    } catch (ClassNotFoundException e) {
      e.printStackTrace();
    } catch (IllegalArgumentException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    } catch (SecurityException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    } catch (IllegalAccessException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    } catch (InvocationTargetException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    } catch (NoSuchMethodException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

}
