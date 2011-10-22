/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Burak Emir
 */

package scala.tools.nsc
package ast.parser

import scala.collection.{ mutable, immutable }
import xml.{ EntityRef, Text }
import xml.XML.{ xmlns }
import symtab.Flags.MUTABLE
import scala.tools.util.StringOps.splitWhere

/** This class builds instance of `Tree` that represent XML.
 *
 *  Note from martin: This needs to have its position info reworked. I don't
 *  understand exactly what's done here. To make validation pass, I set many
 *  positions to be transparent. Not sure this is a good idea for navigating
 *  XML trees in the IDE but it's the best I can do right now. If someone
 *  who understands this part better wants to give it a shot, please do!
 * 
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class SymbolicXMLBuilder(p: Parsers#Parser, preserveWS: Boolean) {
  val global: Global
  import global._

  private[parser] var isPattern: Boolean = _

  private trait XMLTypeNames extends TypeNames {
    val _Comment: NameType             = "Comment"
    val _Elem: NameType                = "Elem"
    val _EntityRef: NameType           = "EntityRef"
    val _Group: NameType               = "Group"
    val _MetaData: NameType            = "MetaData"
    val _NamespaceBinding: NameType    = "NamespaceBinding"
    val _NodeBuffer: NameType          = "NodeBuffer"
    val _PrefixedAttribute: NameType   = "PrefixedAttribute"
    val _ProcInstr: NameType           = "ProcInstr"
    val _Text: NameType                = "Text"
    val _Unparsed: NameType            = "Unparsed"
    val _UnprefixedAttribute: NameType = "UnprefixedAttribute"
  }

  private trait XMLTermNames extends TermNames {
    val _Null: NameType     = "Null"
    val __Elem: NameType    = "Elem"
    val __Text: NameType    = "Text"
    val _buf: NameType      = "$buf"
    val _md: NameType       = "$md"
    val _plus: NameType     = "$amp$plus"
    val _scope: NameType    = "$scope"
    val _tmpscope: NameType = "$tmpscope"
    val _xml: NameType      = "xml"
  }

  private object xmltypes extends XMLTypeNames {
    type NameType = TypeName
    implicit def createNameType(name: String): TypeName = newTypeName(name)
  }
  private object xmlterms extends XMLTermNames {
    type NameType = TermName
    implicit def createNameType(name: String): TermName = newTermName(name)
  }

  import xmltypes.{_Comment, _Elem, _EntityRef, _Group, _MetaData, _NamespaceBinding, _NodeBuffer, 
    _PrefixedAttribute, _ProcInstr, _Text, _Unparsed, _UnprefixedAttribute}
  
  import xmlterms.{_Null, __Elem, __Text, _buf, _md, _plus, _scope, _tmpscope, _xml}

  // convenience methods 
  private def LL[A](x: A*): List[List[A]] = List(List(x:_*))
  private def const(x: Any) = Literal(Constant(x))
  private def wild                          = Ident(nme.WILDCARD)
  private def wildStar                      = Ident(tpnme.WILDCARD_STAR)
  private def _scala(name: Name)            = Select(Select(Ident(nme.ROOTPKG), nme.scala_), name)
  private def _scala_xml(name: Name)        = Select(_scala(_xml), name)
  
  private def _scala_xml_Comment            = _scala_xml(_Comment)
  private def _scala_xml_Elem               = _scala_xml(_Elem)
  private def _scala_xml_EntityRef          = _scala_xml(_EntityRef)
  private def _scala_xml_Group              = _scala_xml(_Group)
  private def _scala_xml_MetaData           = _scala_xml(_MetaData)
  private def _scala_xml_NamespaceBinding   = _scala_xml(_NamespaceBinding)
  private def _scala_xml_NodeBuffer         = _scala_xml(_NodeBuffer)
  private def _scala_xml_Null               = _scala_xml(_Null)
  private def _scala_xml_PrefixedAttribute  = _scala_xml(_PrefixedAttribute)
  private def _scala_xml_ProcInstr          = _scala_xml(_ProcInstr)
  private def _scala_xml_Text               = _scala_xml(_Text)
  private def _scala_xml_Unparsed           = _scala_xml(_Unparsed)
  private def _scala_xml_UnprefixedAttribute= _scala_xml(_UnprefixedAttribute)
  private def _scala_xml__Elem              = _scala_xml(__Elem)
  private def _scala_xml__Text              = _scala_xml(__Text)

  /** Wildly wrong documentation deleted in favor of "self-documenting code." */
  protected def mkXML(
    pos: Position,
    isPattern: Boolean,
    pre: Tree,
    label: Tree,
    attrs: Tree,
    scope:Tree,
    children: Seq[Tree => Tree]): Tree => Tree =
  {
    def starArgs = 
      if (children.isEmpty) (_:Tree) => Nil
      else {
        val seq= makeXMLseq(pos, children)
        (x: Tree) => List(Typed(seq(x), wildStar))
      }

    def pat    = {
      val textPat= convertToTextPat(children)
      (x: Tree) => Apply(_scala_xml__Elem, List(pre, label, wild, wild) ::: textPat.map(_(x)))
    }
    def nonpat = {
      val args= starArgs
      (x: Tree) => New(_scala_xml_Elem, List(List(pre, label, attrs, scope) ::: args(x)))
    }

    givePos(pos) compose { if (isPattern) pat else nonpat }
  }

  final def givePos(pos: Position) = atPos[Tree](pos) _
  
  final def entityRef(pos: Position, n: String) =
    (_: Tree) => atPos(pos)( New(_scala_xml_EntityRef, LL(const(n))) )

  // create scala.xml.Text here <: scala.xml.Node
  final def text(pos: Position, txt: String): Tree => Tree = {
    givePos(pos) compose (if (isPattern) makeTextPat((_:Tree) => const(txt))
    else makeText1(const(txt)))
  }

  def makeTextPat(txt: Tree => Tree)        = (x: Tree) => Apply(_scala_xml__Text, List(txt(x)))
  def makeText1(txt: Tree)                  = (_: Tree) => New(_scala_xml_Text, LL(txt))
  def comment(pos: Position, text: String)  = (_: Tree) => atPos(pos)( Comment(const(text)) )
  def charData(pos: Position, txt: String)  = givePos(pos) compose makeText1(const(txt))
  
  def procInstr(pos: Position, target: String, txt: String) =
    (_: Tree) => atPos(pos)( ProcInstr(const(target), const(txt)) )

  protected def Comment(txt: Tree)                  = New(_scala_xml_Comment, LL(txt))
  protected def ProcInstr(target: Tree, txt: Tree)  = New(_scala_xml_ProcInstr, LL(target, txt))

  /** @todo: attributes */
  def makeXMLpat(pos: Position, n: String, args: Seq[Tree => Tree]): Tree => Tree = {
    val (prepat, labpat) = splitPrefix(n) match {
      case (Some(pre), rest)  => (const(pre), const(rest))
      case _                  => (wild, const(n))
    }
    mkXML(pos, true, prepat, labpat, null, null, args)
  }

  protected def convertToTextPat(t: Tree => Tree): Tree => Tree = t match {
    case _: Literal => makeTextPat(t)
    case _          => t
  }
  protected def convertToTextPat(buf: Seq[Tree => Tree]): List[Tree => Tree] = 
    (buf map convertToTextPat).toList

  def parseAttribute(pos: Position, s: String): Tree => Tree = {
    val ts = xml.Utility.parseAttributeValue(s) map {
      case Text(s)      => text(pos, s)
      case EntityRef(s) => entityRef(pos, s)
    }
    ts.length match {
      case 0 => (_: Tree) => gen.mkNil
      case 1 => ts.head
      case _ => makeXMLseq(pos, ts.toList)
    }
  }

  def isEmptyText(t: Tree) = t match {
    case Literal(Constant("")) => true
    case _ => false
  }

  /** could optimize if args.length == 0, args.length == 1 AND args(0) is <: Node. */
  def makeXMLseq(pos: Position, args: Seq[Tree => Tree]): Tree => Tree = {
    val buffer = ValDef(NoMods, _buf, TypeTree(), New(_scala_xml_NodeBuffer, List(Nil)))
    val applies = (x: Tree) => args map (_(x)) filterNot isEmptyText map (t => Apply(Select(Ident(_buf), _plus), List(t)))

    (t: Tree) => atPos(pos)( Block(buffer :: applies(t).toList, Ident(_buf)) )
  }

  /** Returns (Some(prefix) | None, rest) based on position of ':' */
  def splitPrefix(name: String): (Option[String], String) = splitWhere(name, _ == ':', true) match {
    case Some((pre, rest))  => (Some(pre), rest)
    case _                  => (None, name)
  }

  /** Various node constructions. */
  def group(pos: Position, args: Seq[Tree => Tree]): Tree => Tree = {
    val seq= makeXMLseq(pos, args)
    (x: Tree) => atPos(pos)( New(_scala_xml_Group, LL(seq(x))) )
  }

  def unparsed(pos: Position, str: String): Tree => Tree =
    (_: Tree) => atPos(pos)( New(_scala_xml_Unparsed, LL(const(str))) )

  def element(pos: Position, qname: String, attrMap: mutable.Map[String, Tree => Tree], args: Seq[Tree => Tree]): Tree => Tree = {
    def handleNamespaceBinding(pre: String, z: String): Tree => Tree = {
      def mkAssign(t: Tree => Tree): Tree => Tree = (x: Tree) => Assign(
        Ident(_tmpscope), 
        New(_scala_xml_NamespaceBinding, LL(const(pre), t(x), Ident(_tmpscope)))
      )

      val attr= attrMap(z)
      val uri1 = (t: Tree) => attr(t) match {
        case Apply(_, List(uri @ Literal(Constant(_)))) => mkAssign((_: Tree) => uri)(t)
        case Select(_, nme.Nil)                         => mkAssign((_: Tree) => const(null))(t)  // allow for xmlns="" -- bug #1626
        case x                                          => mkAssign((_: Tree) => x)(t)
      }
      attrMap -= z
      uri1
    }

    /** Extract all the namespaces from the attribute map. */
    val namespaces: List[Tree => Tree] =
      for (z <- attrMap.keys.toList ; if z startsWith xmlns) yield {
        val ns = splitPrefix(z) match {
          case (Some(_), rest)  => rest
          case _                => null
        }
        handleNamespaceBinding(ns, z)
      }

    val (pre, newlabel) = splitPrefix(qname) match {
      case (Some(p), x) => (p, x)
      case (None, x)    => (null, x)
    }

    def mkAttributeTree(pre: String, key: String, value: Tree => Tree) = givePos(pos.makeTransparent) compose {
      // XXX this is where we'd like to put Select(value, nme.toString_) for #1787
      // after we resolve the Some(foo) situation.
      val baseArgs = List((_: Tree) => const(key), value, (_: Tree) => Ident(_md))
      val (clazz, attrArgs) =
        if (pre == null) (_scala_xml_UnprefixedAttribute, baseArgs)
                    else (_scala_xml_PrefixedAttribute  , ((_: Tree) => const(pre)) :: baseArgs)

      (x: Tree) => Assign(Ident(_md), New(clazz, LL(attrArgs.map(_(x)): _*)))
    }

    def handlePrefixedAttribute(pre: String, key: String, value: Tree => Tree)  = mkAttributeTree(pre, key, value)
    def handleUnprefixedAttribute(key: String, value: Tree => Tree)             = mkAttributeTree(null, key, value)

    val attributes: List[Tree => Tree] =
      for ((k, v) <- attrMap.toList.reverse) yield splitPrefix(k) match {
        case (Some(pre), rest)  => handlePrefixedAttribute(pre, rest, v)
        case _                  => handleUnprefixedAttribute(k, v)
      }

    // These three used to be lazy -- presumably for performance reasons. But lazy values
    // used inside a function are only evaluated at evaluation time. Maybe we should evalute
    // them in each of the 4 cases below... 
    val scopeDef     = ValDef(NoMods, _scope, _scala_xml_NamespaceBinding, Ident(_tmpscope))
    val tmpScopeDef  = ValDef(Modifiers(MUTABLE), _tmpscope, _scala_xml_NamespaceBinding, Ident(_scope))
    val metadataDef  = ValDef(Modifiers(MUTABLE), _md, _scala_xml_MetaData, _scala_xml_Null)
    val makeSymbolicAttrs = if (!attributes.isEmpty) Ident(_md) else _scala_xml_Null

    val (attrResult, nsResult): Pair[Tree => List[Tree], Tree => List[Tree]] =
      (attributes.isEmpty, namespaces.isEmpty) match {
        case (true ,  true)   => ((_: Tree) => Nil, (_: Tree) => Nil)
        case (true , false)   => ((_: Tree) => scopeDef :: Nil, (x: Tree) => tmpScopeDef :: namespaces.map(_(x)))
        case (false,  true)   => ((x: Tree) => metadataDef :: attributes.map(_(x)), (_: Tree) => Nil)
        case (false, false)   => ((x: Tree) => scopeDef :: metadataDef :: attributes.map(_(x)), (x: Tree) => tmpScopeDef :: namespaces.map(_(x)))
      }

    val body = mkXML(
      pos.makeTransparent,
      false,
      const(pre),
      const(newlabel),
      makeSymbolicAttrs,
      Ident(_scope),
      args
    )

    (x: Tree) => atPos(pos.makeTransparent)( Block(nsResult(x), Block(attrResult(x), body(x))) )
  }
}
