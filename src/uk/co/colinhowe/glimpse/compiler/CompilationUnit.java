package uk.co.colinhowe.glimpse.compiler;

public abstract class CompilationUnit {
  private final String viewName;
  private final String sourceName;
  
  public CompilationUnit(String viewName, String sourceName) {
    super();
    this.viewName = viewName;
    this.sourceName = sourceName;
  }

  public abstract String getSource();

  public String getViewName() {
    return viewName;
  }
  
  public String getSourceName() {
    return sourceName;
  }
}
