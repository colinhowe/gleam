package uk.co.colinhowe.glimpse.compiler;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class Speed {
  
  private int ITERATIONS = 10000000;
  
  public static List<Object> invokeStatic() {
    return new LinkedList<Object>();
  }
  
  private class GeneratorClass {
    public List<Object> invoke() {
      return new LinkedList<Object>();
    }
  }
  
  public void withInstance() {
    long start = System.currentTimeMillis();
    for (int i = 0; i < ITERATIONS; i++) {
      new GeneratorClass().invoke();
    }
    long end = System.currentTimeMillis();
    System.out.println("With instance took:\t\t" + (end - start) + "ms");
  }
  
  public void withReflection() throws SecurityException, NoSuchMethodException, IllegalArgumentException, IllegalAccessException, InvocationTargetException {
    long start = System.currentTimeMillis();
    for (int i = 0; i < ITERATIONS; i++) {
      Speed.class.getMethod("invokeStatic").invoke(null);
    }
    long end = System.currentTimeMillis();
    System.out.println("Reflection took:\t\t" + (end - start) + "ms");
  }
  
  public void withCachedReflection() throws SecurityException, NoSuchMethodException, IllegalArgumentException, IllegalAccessException, InvocationTargetException {
    long start = System.currentTimeMillis();
    Map<String, Method> methodCache = new HashMap<String, Method>();
    for (int i = 0; i < ITERATIONS; i++) {
      Method method = methodCache.get("invokeStatic");
      if (method != null) {
        method.invoke(null);
      } else {
        method = Speed.class.getMethod("invokeStatic");
        methodCache.put("invokeStatic", method);
        method.invoke(null);
      }
    }
    long end = System.currentTimeMillis();
    System.out.println("Reflection took:\t\t" + (end - start) + "ms");
  }
  
  public void peelingArguments() {
    long start = System.currentTimeMillis();
    Map<String, Object> exampleMap = new HashMap<String, Object>();
    exampleMap.put("hi", 1);
    exampleMap.put("bye", 1);
    exampleMap.put("fly", 1);
    for (int i = 0; i < ITERATIONS; i++) {
      Object hi = exampleMap.get("hi");
      Object bye = exampleMap.get("bye");
      Object fly = exampleMap.get("fly");
      Object smile = exampleMap.get("smile");
    }
    long end = System.currentTimeMillis();
    System.out.println("Reflection took:\t\t" + (end - start) + "ms");
  }
  
  public static void main(String[] args) throws SecurityException, IllegalArgumentException, NoSuchMethodException, IllegalAccessException, InvocationTargetException {
    new Speed().withInstance();
    new Speed().withCachedReflection();
    new Speed().peelingArguments();
    new Speed().withReflection();
  }
}