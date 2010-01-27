package uk.co.colinhowe.glimpse;

import uk.co.colinhowe.glimpse.compiler.typing.*;

public class TypeCheckError extends CompilationError {
  private final Type expectedType;
  private final Type actualType;
  
  public TypeCheckError(int lineNumber, Type expectedType, Type actualType) {
    super(lineNumber);
    this.expectedType = expectedType;
    this.actualType = actualType;
  }
}

