package uk.co.colinhowe.glimpse.compiler;

import java.util.LinkedList;
import java.util.List;

public class DummyController {
  private String name = "Name of the controller";
  
  public static class CompoundProperty {
    private String inner = "Inner property";

    public String getInner() {
      return inner;
    }
  }
  
  private CompoundProperty compound = new CompoundProperty();
  
  private List<String> names = new LinkedList<String>();
  private List<Integer> ages = new LinkedList<Integer>();

  public DummyController() {
    names.add("Alan");
    names.add("Bob");
    names.add("Colin");
    ages.add(18);
    ages.add(21);
    ages.add(25);
  }
  
  
  /**
   * A list of names
   */
  public List<String> getNames() {
    return names;
  }
  
  
  /**
   * A list of ages
   */
  public List<Integer> getAges() {
    return ages;
  }
  

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