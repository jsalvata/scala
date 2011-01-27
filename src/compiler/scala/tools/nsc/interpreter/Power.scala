/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.collection.{ mutable, immutable }
import mutable.{ HashMap }
import scala.tools.nsc.util.{ NoPosition, BatchSourceFile }

/** A class for methods to be injected into the repl in power mode.
 */
class Power(repl: Interpreter) {
  val global: repl.compiler.type = repl.compiler

  import global._
  import definitions.{ getMember, getModule, getClass => getCompilerClass }
  import repl.{ beQuietDuring, interpret, parse }
  
  object phased extends Phased {
    val global: Power.this.global.type = Power.this.global
  }
  
  class ReplSnippet[T](val path: String, initial: T) {
    var code: String = ""
    var value: T = initial
    
    def set(code: String) = interpret(path + ".value = " + code)
    def get: T = value
    override def toString = "repl." + path + ".value = \"" + code + "\""
  }
  
  object vars {
    private def create[T](name: String, initial: T): ReplSnippet[T] =
      new ReplSnippet[T]("power.vars." + name, initial)

    val symfilter = create("symfilter", (s: Symbol) => true)
  }

  def banner = """
    |** Power User mode enabled - BEEP BOOP      **
    |** scala.tools.nsc._ has been imported      **
    |** New vals! Try repl, global, power        **
    |** New cmds! :help to discover them         **
    |** New defs! Type power.<tab> to reveal     **
  """.stripMargin.trim
  
  def init = """
    |import scala.tools.nsc._
    |val global: repl.compiler.type = repl.compiler
    |import global._
    |import definitions._
    |import power.{ phased, show, clazz, module }
  """.stripMargin
  
  /** Starts up power mode and runs whatever is in init.
   */
  def unleash(): Unit = {
    def f = {
      repl.bind[Interpreter]("repl", repl)
      repl.bind[Power]("power", this)
      init split '\n' foreach interpret
    }
    if (isReplDebug) f
    else beQuietDuring { f }
  }

  object show {
    private def defStrings(sym: Symbol, p: Symbol => Boolean) =
      phased(sym.info.members filter p map (_.defString))
  
    private def display(sym: Symbol, p: Symbol => Boolean) =
      defStrings(sym, p) foreach println
    
    def methods[T: Manifest] = display(clazz[T], _.isMethod)
    def apply[T: Manifest] = display(clazz[T], vars.symfilter.get)
  }

  abstract class NameBased[T <: Name] {
    def mkName(s: String): T
    def mkSymbol(s: String): Symbol
    
    def apply[T: Manifest]                 = mkSymbol(manifest[T].erasure.getName)
    def tpe[T: Manifest]                   = apply[T].tpe
    def members[T: Manifest]               = tpe[T].members
    def member[T: Manifest](name: Name)    = getMember(apply[T], name)
    def vmember[T: Manifest](name: String) = member[T](newTermName(name))
    def tmember[T: Manifest](name: String) = member[T](newTypeName(name))
  }
  private def missingWrap(op: => Symbol): Symbol =
    try op
    catch { case _: MissingRequirementError => NoSymbol }
  
  object clazz extends NameBased[TypeName] {
    def mkName(s: String) = newTypeName(s)
    def mkSymbol(s: String): Symbol = missingWrap(getCompilerClass(s))
  }
  object module extends NameBased[TermName] {
    def mkName(s: String) = newTermName(s)
    def mkSymbol(s: String): Symbol = missingWrap(getModule(s))
  }

  def mkContext(code: String = "") = analyzer.rootContext(mkUnit(code))
  def mkAlias(name: String, what: String) = interpret("type %s = %s".format(name, what))
  def mkSourceFile(code: String) = new BatchSourceFile("<console>", code)
  def mkUnit(code: String) = new CompilationUnit(mkSourceFile(code))

  def mkTree(code: String): Tree = mkTrees(code).headOption getOrElse EmptyTree
  def mkTrees(code: String): List[Tree] = parse(code) getOrElse Nil
  def mkTypedTrees(code: String*): List[Tree] = {
    class TyperRun extends Run {
      override def stopPhase(name: String) = name == "superaccessors"
    }

    reporter.reset
    val run = new TyperRun
    run compileSources (code.toList.zipWithIndex map {
      case (s, i) => new BatchSourceFile("<console %d>".format(i), s)
    })
    run.units.toList map (_.body)
  }
  def mkTypedTree(code: String) = mkTypedTrees(code).head
  def mkType(id: String): Type = repl.stringToCompilerType(id)
  
  override def toString = """
    |** Power mode status **
    |Default phase: %s
    |Names: %s
    |Identifiers: %s
  """.stripMargin.format(
      phased.get,
      repl.allreferencedNames mkString " ",
      repl.unqualifiedIds mkString " "
    )
}
