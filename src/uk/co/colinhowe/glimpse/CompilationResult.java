package uk.co.colinhowe.glimpse;

import java.io.File;
import java.util.LinkedList;
import java.util.List;

public class CompilationResult {
  private final String filename;
  private final File clazzFile;
  private final List<CompilationError> errors = new LinkedList<CompilationError>();
  
  public CompilationResult(final String filename, final File clazzFile) {
    this.filename = filename;
    this.clazzFile = clazzFile;
  }
  
  
  public File getClazzFile() {
    return clazzFile;
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
 