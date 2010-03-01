package uk.co.colinhowe.glimpse.compiler;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class FileCompilationUnit extends CompilationUnit {
  final String path;
  
  public FileCompilationUnit(String viewName, String sourceName, String path) {
    super(viewName, sourceName);
    this.path = path;
  }

  public String getSource() {
    BufferedReader reader = null;
    try {
      reader = new BufferedReader(new FileReader(path));
      String line;
      StringBuilder buffer = new StringBuilder();
      while ((line = reader.readLine()) != null) {
        buffer.append(line);
        buffer.append("\n");
      }
      return buffer.toString();
    } catch (Exception e) {
      throw new RuntimeException(e);
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException e) {
          throw new RuntimeException("Failed to close reader", e);
        }
      }
    }
  }
}
