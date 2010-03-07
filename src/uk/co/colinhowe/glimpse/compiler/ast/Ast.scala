package uk.co.colinhowe.glimpse.compiler.ast

case class GNode

case class GImport extends GNode
case class GSingleImport(identifier : String) extends GImport
case class GWildcardImport(identifier : String) extends GImport

case class GController(identifier : String) extends GNode

case class GMacroModifier extends GNode
case class GAbstractMacro extends GMacroModifier
case class GDynamicMacro extends GMacroModifier

case class GArgumentModifier extends GNode
case class GCascade extends GArgumentModifier
case class GRuntimeTyped extends GArgumentModifier

case class GStatement extends GNode
case class GAssignmentStatement(name : String, expression : GExpression) extends GStatement
case class GForLoopStatement(
    iteratorName : String,
    iteratorType : String,
    expression : GExpression,
    statements : GStatements
    ) extends GStatement
case class GIfStatement(
    expression : GExpression,
    trueStatements : GStatements,
    falseStatements : GStatements
    ) extends GStatement
case class GIncludeStatement(
    name : String,
    arguments : List[GNamedArgument]
    ) extends GStatement
case class GIncrementStatement(name : String) extends GStatement
case class GMacroStatement(
    name : String,
    arguments : List[GNamedArgument],
    value : GExpression) extends GStatement
case class GNodeStatement(
    name : String,
    arguments : List[GNamedArgument],
    value : GExpression) extends GStatement
case class GVariableDefinitionStatement(
    name : String,
    value : GExpression) extends GStatement

case class GNamedArgument(name : String, value : GExpression) extends GNode

case class GStatements(statements : List[GStatement]) extends GNode

case class GGenerator(
    arguments : List[GNamedArgumentDefinition],
    statements : GStatements
    ) extends GNode

case class GExpression extends GNode
case class GConstantExpression(value : Int) extends GExpression
case class GControllerMethodExpression(
    name : String, 
    arguments : List[GExpression]) extends GExpression
case class GBooleanExpression(value : Boolean) extends GExpression
case class GGeneratorExpression(generator : GGenerator) extends GExpression
case class GInvertExpression(expression : GExpression) extends GExpression
case class GPropertyExpression(path : List[String]) extends GExpression
case class GControllerPropertyExpression(path : List[String]) extends GExpression
case class GPropertyReferenceExpression(path : List[String]) extends GExpression
case class GStringExpression(value : String) extends GExpression

case class GWithDefinition(
    contentName : String,
    contentType : String,
    arguments : List[GNamedArgumentDefinition]
    ) extends GNode

case class GNamedArgumentDefinition(
    name : String,
    modifiers : List[GArgumentModifier],
    argumentType : String,
    default : GExpression
    ) extends GNode

case class GMacroDefn(
    name : String, 
    modifier : GMacroModifier,
    generics : List[String],
    arguments : List[GNamedArgumentDefinition],
    withDefinition : GWithDefinition,
    controller : GController,
    restrictions : List[String],
    generator : GGenerator
    ) extends GNode

case class GView(
    imports : List[GImport], 
    controller : GController,
    macroDefinitions : GMacroDefn,
    body : GStatements
    ) extends GNode