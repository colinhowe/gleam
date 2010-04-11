package uk.co.colinhowe.gleam.compiler

import java.io.FileInputStream
import java.util.jar.JarInputStream
import java.io.File
import scala.collection.mutable.{ListBuffer => MList}
import scala.collection.mutable.{Map => MMap}
import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.Futures._

object ClassPathResolver {
  case class Resolved()
}

class ClassPathResolver(paths : Iterable[String], notify : Actor) {
  private val defaultPackage = new Package
  
  // TODO - Doing this as lazily as possible would be neat
  processPaths(paths)
  
  private class Package {
    val subPackages : MMap[String, Package] = MMap[String, Package]()
    val classes : MList[String] = MList[String]()
  }
  
  def isClassOnClassPath(qualifiedName : String) : Boolean = {
    val tokens = qualifiedName.split("\\.")
    val className = tokens.last
    val packages = tokens.dropRight(1)
    
    var currentPackage = defaultPackage
    for (p <- packages) {
      if (currentPackage.subPackages.contains(p)) {
        currentPackage = currentPackage.subPackages(p)
      } else {
        return false
      }
    }
    
    return currentPackage.classes.contains(className)
  }
  
  def getAllClassesInPackage(packageName : String) : List[String] = {
    val tokens = packageName.split("\\.")
    val p = tokens.foldLeft(defaultPackage)(_.subPackages(_))
    p.classes.toList
  }
  
  private def processPaths(paths : Iterable[String]) {
    future {
      for (path <- paths) {
        processPath(path)
      }
      notify ! ClassPathResolver.Resolved
    }
  }
  
  private def processPath(path : String) {
    if (path.endsWith(".jar")) {
      processJar(path)
    } else {
      processDirectory(path, "")
    }
  }
  
  
  private def processJar(jarPath : String) {
    val jarFile = new JarInputStream(new FileInputStream(jarPath))

    try {
      var continue = true
      while (continue) {
        val jarEntry = jarFile.getNextJarEntry()
        if (jarEntry == null) {
          continue = false
        } else {
          if (jarEntry.getName().endsWith(".class")) {
            addClass(jarEntry.getName())
          }
        }
      }
    } finally {
      jarFile.close()
    }
  }
  
  private def processDirectory(path : String, parentPath : String) {
    // Go over the classes in the path and create packages as needed
    val directory = new File(path)
    for (file <- directory.listFiles) {
      if (file.isDirectory) {
        processDirectory(file.getPath(), parentPath + file.getName() + "/")
      } else {
        if (file.getName().endsWith(".class")) {
          addClass(parentPath + file.getName())
        }
      }
    }
  }
  
  private def addClass(classPath : String) {
    val tokens = classPath.split("/")
    val className = tokens.last.substring(0, tokens.last.size - 6) // Remove .class
    val packages = tokens.dropRight(1)

    var currentPackage = defaultPackage
    for (p <- packages) {
      if (!currentPackage.subPackages.contains(p)) {
        currentPackage.subPackages(p) = new Package
      }
      currentPackage = currentPackage.subPackages(p)
    }
    
    currentPackage.classes += className
  }
}