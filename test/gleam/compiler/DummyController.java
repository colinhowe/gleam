package gleam.compiler;

import java.math.BigInteger;
import java.util.LinkedList;
import java.util.List;

public class DummyController implements DummyInterface {
  private String name = "Name of the controller";
  
  private CompoundProperty compound = new CompoundProperty();
  
  private List<String> names = new LinkedList<String>();
  private List<Integer> ages = new LinkedList<Integer>();

  private Object someString = "someString";
  private Object someBigInteger = new BigInteger("5");


  public DummyController() {
    names.add("Alan");
    names.add("Bob");
    names.add("Colin");
    ages.add(18);
    ages.add(21);
    ages.add(25);
  }
  
  public String makeMessage(String id) {
    return id + ".message";
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
  
  public Object getSomeString() {
    return someString;
  }


  public Object getSomeBigInteger() {
    return someBigInteger;
  }
}