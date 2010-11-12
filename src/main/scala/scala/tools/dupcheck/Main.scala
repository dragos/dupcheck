package scala.tools.dupcheck

import scala.tools.nsc.io._
import scala.collection.mutable

object Main {
  var javap: Boolean = false
  var checkDups: Boolean = false
  var fileName = ""
  
  var exitCode = 0;
    
  def main(args: Array[String]) {
    parseArgs(args)
    
    try {
      val file = AbstractFile.getFile(fileName)
      val parser = new ClassfileParser
      val clazz = parser.parse(file)
      
      if (javap) 
        printClass(clazz)
        
      if (checkDups) {
        checkDuplicates(clazz.fields)
        checkDuplicates(clazz.methods)
      }
      exit(exitCode)
    } catch {
      case e: java.io.IOException => 
        println("Error reading file %s: %s".format(args(1), e.getMessage))
    }
  }
  
  def checkDuplicates(mbrs: List[MemberDef]) {
    val reported: mutable.Set[MemberDef] = mutable.Set()
    
    for (f1 <- mbrs; f2 <- mbrs; if f1 ne f2)
      if (f1.name == f2.name && f1.descriptor == f2.descriptor && !reported(f1)) {
        println("Duplicate member: %s\n\t%s\n\t%s".format(f1.name, f1, f2))
        reported += (f1, f2)
        exitCode = 1
      }
  }
  
  def printClass(clazz: ClassDef) {
    print("class %s extends %s".format(clazz.name, clazz.superclass))
    if (clazz.interfaces.nonEmpty) print(clazz.interfaces.mkString(" with ", " with ", " {"))
    else println(" {")
    println("\t%s".format(clazz.javasig.getOrElse("")))
    println
    
    println(clazz.fields.mkString("  ", "\n  ", ""))
    println
    println(clazz.methods.mkString("  ", "\n  ", ""))
    println("}")
  }
  
  def parseArgs(args: Array[String]) {
    for (a <- args) a match {
      case "-p" => javap = true
      case "-d" => checkDups = true
      case "-help" => usage()
      case f => fileName = f 
    }
    
    if (!javap && !checkDups) usage()
  }
  
  def usage() {
    println(
"""Usage: dupcheck <classfile>
    -p             Print member information
    -d             Check for duplicate members
""")
    exit(1)
  }
}
