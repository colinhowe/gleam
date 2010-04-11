package uk.co.colinhowe.gleam.compiler;

import java.io.Reader;
import java.io.StringReader;

public class StringCompilationUnit extends CompilationUnit {
  private final String source;
  
  public StringCompilationUnit(String viewName, String sourceName, String source) {
    super(viewName, sourceName);
    this.source = source;
  }

  public Reader getReader() {
    return new StringReader(source);
  }
  
  public String toString() {
    return "StringCompilationUnit(" + this.getViewName() + ")";
  }
}
