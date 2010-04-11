package uk.co.colinhowe.gleam.compiler;

import java.io.Reader;

public abstract class CompilationUnit {
  private final String viewName;
  private final String sourceName;
  
  public CompilationUnit(String viewName, String sourceName) {
    super();
    this.viewName = viewName;
    this.sourceName = sourceName;
  }

  public abstract Reader getReader();

  public String getViewName() {
    return viewName;
  }
  
  public String getSourceName() {
    return sourceName;
  }
}
