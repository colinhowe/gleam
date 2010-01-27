package uk.co.colinhowe.glimpse;

@SuppressWarnings("serial")
public class IdentifierNotFoundException extends RuntimeException {
  final String identifier;
  
  public String getIdentifier() {
    return identifier;
  }

  public IdentifierNotFoundException(String identifier) {
    this.identifier = identifier;
  }
  
}