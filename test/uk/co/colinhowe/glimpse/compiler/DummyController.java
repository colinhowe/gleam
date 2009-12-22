package uk.co.colinhowe.glimpse.compiler;

public class DummyController {
  private String name = "Name of the controller";
  
  public static class CompoundProperty {
    private String inner = "Inner property";

    public String getInner() {
      return inner;
    }
  }
  
  private CompoundProperty compound = new CompoundProperty();
  

  /**
   * A compound property
   */
  public CompoundProperty getCompound() {
    return compound;
  }

  /**
   * Get the name of the controller
   */
  public String getName() {
    return name;
  }
}