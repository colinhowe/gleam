package uk.co.colinhowe.glimpse.compiler

import java.io.File
import scala.collection.JavaConversions._

object PerformanceTest {
  def main(args : Array[String]) {
    val tempFolder = new File("temp/")
    if (!tempFolder.exists) {
      tempFolder.mkdir()
      tempFolder.deleteOnExit
    }
    
    // Build up a list of compilation units
    val compilationUnits = scala.collection.mutable.ListBuffer[CompilationUnit]()
    for (i <- 0 until 100) {
      val compilationUnit = new FileCompilationUnit("v" + i, "source" + i, "performance/v" + i + ".glimpse")
      compilationUnits += compilationUnit
    }

    val start = System.currentTimeMillis
    for (i <- 0 until 20) {
      new GlimpseCompiler().compile(compilationUnits, scala.collection.mutable.Buffer[String]("bin"))
    }
    val end = System.currentTimeMillis
    System.out.println((end - start) + "\tms")
  }
}
