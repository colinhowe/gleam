package gleam.compiler;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.Reader;

public class FileCompilationUnit extends CompilationUnit {
  final String path;
  
  public FileCompilationUnit(String viewName, String sourceName, String path) {
    super(viewName, sourceName);
    this.path = path;
  }

  public Reader getReader() {
    try {
      return new BufferedReader(new FileReader(path));
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
  
  public String toString() {
    return "File(" + path + ")";
  }
}
