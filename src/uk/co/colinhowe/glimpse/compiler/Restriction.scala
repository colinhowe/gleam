package uk.co.colinhowe.gleam.compiler

abstract class Restriction

case class NameRestriction(val name : String) extends Restriction