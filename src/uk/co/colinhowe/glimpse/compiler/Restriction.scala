package uk.co.colinhowe.glimpse.compiler

abstract case class Restriction

case class NameRestriction(val name : String) extends Restriction