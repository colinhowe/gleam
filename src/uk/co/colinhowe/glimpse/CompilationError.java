package uk.co.colinhowe.glimpse;

public abstract class CompilationError {
  private final int lineNumber;
  
  public CompilationError(final int lineNumber) {
    this.lineNumber = lineNumber;
  }

  public int getLineNumber() {
    return lineNumber;
  }
}
