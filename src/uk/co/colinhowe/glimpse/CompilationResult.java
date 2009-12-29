package uk.co.colinhowe.glimpse;

import java.util.LinkedList;
import java.util.List;

public class CompilationResult {
  private final String filename;
  private final List<CompilationError> errors = new LinkedList<CompilationError>();
  
  public CompilationResult(final String filename) {
    this.filename = filename;
  }
  
  
  public void addError(final CompilationError error) {
    this.errors.add(error);
  }
  
  
  public List<CompilationError> getErrors() {
    return errors;
  }
  
  
  public String getFilename() {
    return filename;
  }
}
 