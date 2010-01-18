package uk.co.colinhowe.glimpse.compiler;

public class CompilationUnit {
  private final String source;
  private final String viewName;
  
  public CompilationUnit(String viewName, String source) {
    super();
    this.source = source;
    this.viewName = viewName;
  }

  public String getSource() {
    return source;
  }

  public String getViewName() {
    return viewName;
  }
  
  
}
