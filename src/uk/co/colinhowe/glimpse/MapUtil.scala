package uk.co.colinhowe.glimpse

class MapUtil {
  def asImmutable[A, B](map : scala.collection.mutable.Map[A, B]) = {
    Map() ++ map
  }
}