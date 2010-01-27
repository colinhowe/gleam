package uk.co.colinhowe.glimpse;

public class IdentifierNotFoundError extends CompilationError {
  private final String identifier;
  
  public String getIdentifier() {
    return identifier;
  }

  public IdentifierNotFoundError(int lineNumber, String identifier) {
    super(lineNumber);
    this.identifier = identifier;
  }
}
