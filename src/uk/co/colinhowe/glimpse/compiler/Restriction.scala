package uk.co.colinhowe.glimpse.compiler

abstract class Restriction

case class NameRestriction(val name : String) extends Restriction