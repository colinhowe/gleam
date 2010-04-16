package gleam

class PropertyReference[T](val path : String, val value : T) {

  def getPath = path
  def getValue = value
  
  def set(controller : Object, value : Object) {
    
    // Parse the path
    val elements = path.split("\\.")

    /*
     * Go over each element in the path except the last
     * to find the eventual owner of the property.
     * E.g:
     *   a.b.c
     *     ^
     *     Property owner
     */   
    var currentOwner = controller
    for (i <- 0 until elements.size - 1) {
      val getter = currentOwner.getClass().getMethod("get" + capitalise(elements(i)))
      currentOwner = getter.invoke(currentOwner)
    }
    
    // Set the final element
    val methods = currentOwner.getClass().getMethods()
    for (method <- methods) {
      if (method.getName == "set" + capitalise(elements(elements.size - 1))) {
        method.invoke(currentOwner, value)
      }
    }
  }
  
  private def capitalise(s : String) = {
    s.substring(0, 1).toUpperCase + s.substring(1)
  }
}