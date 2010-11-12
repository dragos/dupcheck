package scala.tools.dupcheck

import scala.tools.nsc.symtab.classfile.ClassfileConstants._

/** Member definitions in classfiles.
 */
abstract class MemberDef(val name: String, val descriptor: String, val flags: Int, val javasig: Option[String]) {
  def kind: String
  
  def hasFlag(f: Int) = (flags & f) != 0
  
  def flagToString(flag: Int): String = flag match {
    case JAVA_ACC_PUBLIC => "public"
    case JAVA_ACC_PRIVATE => "private"
    case JAVA_ACC_PROTECTED => "protected"
    case JAVA_ACC_STATIC => "static"
    case JAVA_ACC_FINAL => "final"
    case JAVA_ACC_SUPER => "<super>/synchronized"
    case JAVA_ACC_VOLATILE => "volatile/<bridge>"
    case JAVA_ACC_TRANSIENT => "transient/<varargs>"
    case JAVA_ACC_NATIVE => "native"
    case JAVA_ACC_INTERFACE => "interface"
    case JAVA_ACC_ABSTRACT => "abstract"
    case JAVA_ACC_STRICT => "strict"
    case JAVA_ACC_SYNTHETIC => "<synthetic>"
    case JAVA_ACC_ANNOTATION => "annotation"
    case JAVA_ACC_ENUM => "enum"
    case _ => ""
  }
  
  def flagsToString: String =
    (for (step <- 0 to 31) yield flagToString(flags & (1 << step))).filter(_ != "").mkString("", " ", "")
  
  override def toString =
    "%s %s %s: %s\n\t%s".format(flagsToString, kind, name, descriptor, javasig.getOrElse(""))
}

case class FieldDef(override val name: String, 
                    override val descriptor: String, 
                    override val flags: Int, 
                    override val javasig: Option[String]) extends MemberDef(name, descriptor, flags, javasig) {
  def kind = "var"
}

case class MethodDef(override val name: String, 
                     override val descriptor: String, 
                     override val flags: Int,
                     override val javasig: Option[String]) extends MemberDef(name, descriptor, flags, javasig) {
  def kind = "def"
}

case class ClassDef(name: String, 
                    superclass: String, 
                    interfaces: List[String], 
                    fields: List[FieldDef], 
                    methods: List[MethodDef],
                    val javasig: Option[String]) {
  override def toString = "%s extends %s\n\t%s".format(name, (superclass :: interfaces).mkString("", " with ", ""), javasig.getOrElse("")) 
}
