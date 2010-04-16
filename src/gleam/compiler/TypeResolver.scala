package gleam.compiler
import gleam.compiler.analysis.DepthFirstAdapter
import gleam.compiler.node._
import gleam.compiler.typing.Type
import scala.collection.JavaConversions._
import scala.actors.Actor._

class TypeResolver(
    val typeProvider : TypeProvider, 
    val macroProvider : MacroDefinitionProvider
  ) extends DepthFirstAdapter {

  private val types = scala.collection.mutable.Map[Node, Type]()
  
  case class Stop()
  val typesActor = actor {
    loop {
      react {
        case (node : Node, t : Type) =>
          types.put(node, t)
        case Stop() =>
          reply(null)
          exit
      }
    }
  }
  
  def addType(node : Node, t : Type) {
    types.put(node, t)
  }
  
  def getType(node : Node, typeNameResolver : TypeNameResolver, additionalTypes : Map[String, Type] = Map()) = {
    if (types.contains(node)) {
      types(node)
    } else {
      typeProvider.getType(node, typeNameResolver, additionalTypes)
    }
  }
  
  
  override def outAPropertyExpr(node : APropertyExpr) {
    val name = IdentifierConverter.identifierListToString(node.getIdentifier)
    if (macroProvider.get(name).size > 0) {
      typesActor ! (node, macroProvider.get(name).iterator.next())
    } else {
      // TODO Get the type out
    }
  }
  
  def stop : Unit = { 
    typesActor !? Stop()
  }
}