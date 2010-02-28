package uk.co.colinhowe.glimpse.infrastructure;

import uk.co.colinhowe.glimpse.Macro;
import uk.co.colinhowe.glimpse.Node;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

public abstract class RuntimeTypedMacro implements Macro {
  protected abstract String getRuntimeTypedArgumentName();
  protected abstract String getMacroName();
  
  private Map<Class<?>, Macro> cache = new HashMap<Class<?>, Macro>();
  
  public java.util.List<Node> invoke(
      Scope scope, 
      java.util.Map<String, Object> arguments, 
      Object value) {
    
    final Object runtimeTypedArgument = arguments.get(getRuntimeTypedArgumentName());

    Class<?> currentClass = runtimeTypedArgument.getClass();
    
    while (currentClass != null) {
      // Check the cache for a match
      final Macro cachedMacro = cache.get(currentClass);
      if (cachedMacro != null) {
        return cachedMacro.invoke(scope, arguments, value);
      } else {
        // Attempt to get a macro that has the current class as the
        // run time typed argument
        String macroName = getMacroName() + "$rtt" 
          + Integer.toString(Math.abs(currentClass.getCanonicalName().hashCode()), 16); 
        
        Class<?> clazz = attemptClassWithName(macroName);
        
        if (clazz != null) {
          // Call the static getInstance method 
          try {
            Method method = clazz.getMethod("getInstance");
            Macro macro = (Macro)method.invoke(null);
            
            // This macro needs caching
            synchronized(cache) {
              cache.put(runtimeTypedArgument.getClass(), macro);
            }
            
            return macro.invoke(scope, arguments, value);
          } catch (Exception e) {
            throw new RuntimeException(e);
          }
        } else {
          currentClass = currentClass.getSuperclass();
        }
      }
    }

    throw new RuntimeException(
        "No concrete implementation of " + 
        getMacroName() + 
        " with runtime type " +
        runtimeTypedArgument.getClass().getCanonicalName() + 
        " exists for argument " +
        getRuntimeTypedArgumentName());
  }
  
  protected Class<?> attemptClassWithName(String className) {
    try {
      return this.getClass().getClassLoader().loadClass(className);
    } catch (ClassNotFoundException e) {
      return null;
    }
  }
}
