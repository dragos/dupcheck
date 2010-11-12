package scala.tools.dupcheck

/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 * @author Iulian Dragos
 */

import java.io.IOException
import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.symtab.classfile.ClassfileConstants._
import scala.annotation.switch
import scala.tools.nsc.io._
import scala.io.UTF8Codec
import scala.collection.immutable


/** This abstract class implements a class file parser.
 *
 *  @author Martin Odersky
 *  @author Iulian Dragos
 */
class ClassfileParser {

var in: BufferReader = _  // the class file reader
  private var thepool: ConstantPool = _

  var fields: List[FieldDef] = Nil
  var methods: List[MethodDef] = Nil
  var superclass: String = _
  var interfaces: List[String] = Nil
  var file: AbstractFile = _
  
  def pool: ConstantPool = thepool

  def parse(file: AbstractFile): ClassDef = {
    this.file = file
    
    def handleError(e: Exception) = {
      throw e;
      throw new IOException("class file '" + file + "' is broken\n(" + {
        if (e.getMessage() != null) e.getMessage()
        else e.getClass.toString
      } + ")")
    }

    in = new BufferReader(file.toByteArray)
    try {
      parseAll()
    } catch {
      case e: RuntimeException => handleError(e)
    }
  } 

  protected def parseAll(): ClassDef = {
    parseHeader()
    thepool = new ConstantPool
    parseClass()
  }

  protected def parseHeader() {
    val magic = in.nextInt
    if (magic != JAVA_MAGIC)
      throw new IOException("class file '" + file + "' "
                            + "has wrong magic number 0x" + magic.toHexString
                            + ", should be 0x" + JAVA_MAGIC.toHexString)
    val minorVersion = in.nextChar.toInt
    val majorVersion = in.nextChar.toInt
  }

  class ConstantPool {
    val length = in.nextChar
    private val starts = new Array[Int](length)
    private val values = new Array[AnyRef](length)
    private val internalized = new Array[String](length)
    
    { var i = 1
      while (i < length) {
        starts(i) = in.bp
        i += 1
        (in.nextByte.toInt: @switch) match {
          case CONSTANT_UTF8 | CONSTANT_UNICODE =>
            in.skip(in.nextChar)
          case CONSTANT_CLASS | CONSTANT_STRING =>
            in.skip(2)
          case CONSTANT_FIELDREF | CONSTANT_METHODREF | CONSTANT_INTFMETHODREF
             | CONSTANT_NAMEANDTYPE | CONSTANT_INTEGER | CONSTANT_FLOAT =>
            in.skip(4)
          case CONSTANT_LONG | CONSTANT_DOUBLE =>
            in.skip(8)
            i += 1
          case _ =>
            errorBadTag(in.bp - 1)
        }
      }
    }

    /** Return the name found at given index. */
    def getName(index: Int): String = {
      if (index <= 0 || length <= index) errorBadIndex(index)
      var name = values(index).asInstanceOf[String]
      if (name eq null) {
        val start = starts(index)
        if (in.buf(start).toInt != CONSTANT_UTF8) errorBadTag(start)
        name = UTF8Codec.decode(in.buf, start + 3, in.getChar(start + 1))
        values(index) = name
      }
      name
    }

    /** Return the name found at given index in the constant pool, with '/' replaced by '.'. */
    def getExternalName(index: Int): String = {
      if (index <= 0 || length <= index) errorBadIndex(index)
      if (internalized(index) eq null) {
        internalized(index) = getName(index).replace('/', '.')
      }
      internalized(index)
    }

    def getClassInfo(index: Int): String = {
      if (index <= 0 || length <= index) errorBadIndex(index)
      var c = values(index).asInstanceOf[String]
      if (c eq null) {
        val start = starts(index)
        if (in.buf(start).toInt != CONSTANT_CLASS) errorBadTag(start)
        val name = getExternalName(in.getChar(start + 1))
        c = name.replace("/", ".")
        //if (c == ClassInfo.NoClass) println("warning: missing class "+name+" referenced from "+parsedClass.file)
        values(index) = c
      }
      c
    }
    
    /** Return the external name of the class info structure found at 'index'.
     *  Use 'getClassSymbol' if the class is sure to be a top-level class.
     */
    def getClassName(index: Int): String = {
      val start = starts(index)
      if (in.buf(start).toInt != CONSTANT_CLASS) errorBadTag(start)
      getExternalName(in.getChar(start + 1))
    }
    
    def getSuperClass(index: Int): String =
      if (index == 0) "java.lang.Object" else getClassInfo(index)

    /** Return a name string and a type string at the given index. If the type is a method
     *  type, a dummy symbol is created in 'ownerTpe', which is used as the
     *  owner of its value parameters. This might lead to inconsistencies,
     *  if a symbol of the given name already exists, and has a different
     *  type. 
     */
    def getNameAndType(index: Int): (String, String) = {
      if (index <= 0 || length <= index) errorBadIndex(index)
      var p = values(index).asInstanceOf[(String, String)]
      if (p eq null) {
        val start = starts(index)
        if (in.buf(start).toInt != CONSTANT_NAMEANDTYPE) errorBadTag(start)
        val name = getName(in.getChar(start + 1).toInt)
        var tpe  = getName(in.getChar(start + 3).toInt)
        p = (name, tpe)
      }
      p
    }

    /** Throws an exception signaling a bad constant index. */
    private def errorBadIndex(index: Int) =
      throw new RuntimeException("bad constant pool index: " + index + " at pos: " + in.bp)

    /** Throws an exception signaling a bad tag at given address. */
    private def errorBadTag(start: Int) =
      throw new RuntimeException("bad constant pool tag " + in.buf(start) + " at byte " + start)
  }

  def parseFields(): Unit = 
    fields = parseMembers[FieldDef](FieldDef)

  def parseMethods() = 
    methods = parseMembers(MethodDef)
            
  def parseMembers[T <: MemberDef](ctor: (String, String, Int, Option[String]) => T): List[T] = {
    val memberCount = in.nextChar
    var members = new ArrayBuffer[T]
    for (i <- 0 until memberCount) {
      val jflags = in.nextChar.toInt
      members += parseMember(jflags, ctor)
    }
    members.toList
  }

  def skipMembers() {
    val memberCount = in.nextChar
    for (i <- 0 until memberCount) {
      in.skip(6); skipAttributes()
    }
  }
  
  def parseMember[T <: MemberDef](jflags: Int, ctor: (String, String, Int, Option[String]) => T): T = {
    val name = pool.getName(in.nextChar)
    val sig = pool.getExternalName(in.nextChar)
    val attrs = parseAttributes

    ctor(name, sig, jflags, attrs.get("Signature"))
  }

  def parseClass(): ClassDef = {
    var flags = in.nextChar
    val nameIdx = in.nextChar
    val externalName = pool.getClassName(nameIdx)

    def parseSuperClass(): String = 
      pool.getClassName(in.nextChar)

    def parseInterfaces(): List[String] = {
      val rawInterfaces = 
        for (i <- List.range(0, in.nextChar)) yield pool.getClassName(in.nextChar)
      rawInterfaces
    }

    superclass = parseSuperClass()
    interfaces = parseInterfaces()
    parseFields()
    parseMethods()
    val attrs = parseAttributes
    ClassDef(externalName.replace('/', '.'), superclass, interfaces, fields, methods, attrs.get("Signature"))
  }

  def skipAttributes() {
    val attrCount = in.nextChar
    for (i <- 0 until attrCount) {
      in.skip(2); in.skip(in.nextInt)
    }
  }

  /** Return true iff TraitSetter annotation found among attributes */
  def parseAttributes: immutable.Map[String, String] = {
    var attrs = new collection.immutable.ListMap[String, String]
    
    val attrCount = in.nextChar
    for (i <- 0 until attrCount) {
      val attrIndex = in.nextChar
      val attrName = pool.getName(attrIndex)
      val attrLen = in.nextInt
      val attrEnd = in.bp + attrLen
      if (attrName == "Signature")
        attrs += attrName -> pool.getName(in.nextChar)
      in.bp = attrEnd
    }
    attrs
  }

  /** Skip a single annotation
   */
  def skipAnnotation(annotIndex: Int, attrEnd: Int) {
    try {
      if (in.bp + 2 <= attrEnd) {
        val nargs = in.nextChar
        for (i <- 0 until nargs)
          if (in.bp + 2 <= attrEnd) {
            val argname = in.nextChar
            skipAnnotArg(attrEnd)
          }
      }
    } catch {
      case ex: Exception => 
    }
  }

  /** Skip a single annotation argument
   */
  def skipAnnotArg(attrEnd: Int) {
    if (in.bp + 3 <= attrEnd) {
      val tag = in.nextByte.toChar
      val index = in.nextChar
      tag match {
        case ENUM_TAG   =>
          if (in.bp + 2 <= attrEnd) in.nextChar
        case ARRAY_TAG  =>
          for (i <- 0 until index)
            skipAnnotArg(attrEnd)
        case ANNOTATION_TAG =>
          skipAnnotation(index, attrEnd)
        case _ =>
      }
    }
  }
}
