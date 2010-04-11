package uk.co.colinhowe.glimpse.tools;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.LinkedList;
import java.util.List;

import uk.co.colinhowe.glimpse.Node;
import uk.co.colinhowe.glimpse.View;
import uk.co.colinhowe.glimpse.compiler.CompilationUnit;
import uk.co.colinhowe.glimpse.compiler.GlimpseCompiler;
import uk.co.colinhowe.glimpse.compiler.StringCompilationUnit;

public class GlimpseRunner {
  
  /**
   * Runs the view with the given name with the given controller.
   * 
   * <p>If controller is null then a controller will not be passed to the view.</p>
   * 
   * @param filename
   * @param controller
   * 
   * @return
   */
  @SuppressWarnings({ "deprecation", "unchecked" })
  public List<Node> run(String filename, Object controller) {
    try {
      final GlimpseCompiler compiler = new GlimpseCompiler();
      
      final BufferedReader reader = new BufferedReader(new FileReader(filename));
      final StringBuffer lines = new StringBuffer();
      String line;
      while ((line = reader.readLine()) != null) {
        lines.append(line + "\n");
      }
      
      // Get the name of the file without any extension
      File file = new File(filename);
      String viewname = file.getName();
      viewname = viewname.substring(0, viewname.indexOf("."));
      
      // Compile the view
      List<CompilationUnit> units = new LinkedList<CompilationUnit>();
      units.add(new StringCompilationUnit(viewname, filename, lines.toString()));
      
      compiler.compile(units);
      
      // Run the view
      URLClassLoader loader = new URLClassLoader(new URL[] { new File("temp/").toURL() });
      Class<? extends View> viewClazz = (Class<? extends View>)loader.loadClass(viewname);
      return viewClazz.newInstance().view(null);
    } catch (Exception e) {
      throw new RuntimeException("Failed to run view in stream", e);
    }
  }
}
