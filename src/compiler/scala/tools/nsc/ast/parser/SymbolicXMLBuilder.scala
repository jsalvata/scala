/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Burak Emir
 */

package scala.tools.nsc
package ast.parser

import scala.collection.{ mutable, immutable }
import scala.reflect.internal.Flags
import xml.{ EntityRef, Text }
import xml.XML.{ xmlns }
import symtab.Flags.MUTABLE
import scala.tools.util.StringOps.splitWhere
import scala.collection.mutable.ListBuffer

/** This class builds instance of `Tree` that represent XML.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class SymbolicXMLBuilder(p: Parsers#Parser, preserveWS: Boolean) {
  val global: Global
  import global._

  private[parser] var isPattern: Boolean = _

  private trait XMLTypeNames extends TypeNames {
    val _NamespaceBinding:      NameType = "NamespaceBinding"
    val _XMLUnmarshaller:       NameType = "XMLUnmarshaller"
  }

  private trait XMLTermNames extends TermNames {
    val _None:                  NameType = "None"
    val _Some:                  NameType = "Some"

    val _xml: NameType      = "xml"

    val _xmlUnmarshaller:       NameType = "$xmlUnmarshaller"

    // The following are named after SAX2 methods
    val _startDocument:         NameType = "startDocument"
    val _endDocument:           NameType = "endDocument"
    val _startElement:          NameType = "startElement"
    val _endElement:            NameType = "endElement"
    val _characters:            NameType = "characters"
    val _comment   :            NameType = "comment"
    val _startCDATA:            NameType = "startCDATA"
    val _endCDATA  :            NameType = "endCDATA"
    val _skippedEntity:         NameType = "skippedEntity" // TODO: do the semantics of this method match SAX2?
    val _processingInstruction: NameType = "processingInstruction"
    val _startPrefixMapping:    NameType = "startPrefixMapping"
    val _endPrefixMapping:      NameType = "endPrefixMapping" // Signals end of mapping value (uri), NOT end of scope as in SAX2
      
    // These behave similarly, but have no mapping in SAX2
    // (because they are not general XML, but specific to Scala)
    val _elementPattern:        NameType = "elementPattern"
    val _embeddedExpression:    NameType = "embeddedExpression"
    val _unparsed:              NameType = "unparsed"
    val _startGroup:            NameType = "startGroup"
    val _endGroup:              NameType = "endGroup"
      
    // These don't have a mapping in SAX2 either,
    // from design preference. TODO: I still have doubts.
    val _startAttribute:        NameType = "startAttribute"
    val _endAttribute:          NameType = "endAttribute"
  }

  private trait XMLMethodNames {
    
  }
  
  private object xmltypes extends XMLTypeNames {
    type NameType = TypeName
    implicit def createNameType(name: String): TypeName = newTypeName(name)
  }
  private object xmlterms extends XMLTermNames {
    type NameType = TermName
    implicit def createNameType(name: String): TermName = newTermName(name)
  }

  import xmltypes.{_NamespaceBinding, _XMLUnmarshaller }
  
  import xmlterms.{_None, _Some, _xml, _xmlUnmarshaller,
    _startDocument, _endDocument, _startElement, _endElement, _elementPattern,
    _characters, _comment, _startCDATA, _endCDATA, _skippedEntity, _processingInstruction,
    _startPrefixMapping, _endPrefixMapping,
    _embeddedExpression, _unparsed, _startGroup, _endGroup,
    _startAttribute, _endAttribute}

  // convenience methods 
  private def LL[A](x: A*): List[List[A]] = List(List(x:_*))
  private def const(x: Any) = Literal(Constant(x))
  private def wild                          = Ident(nme.WILDCARD)
  private def wildStar                      = Ident(tpnme.WILDCARD_STAR)
  private def _scala(name: Name)            = Select(Select(Ident(nme.ROOTPKG), nme.scala_), name)
  private def _scala_xml(name: Name)        = Select(_scala(_xml), name)

  private def _scala_None                   = _scala(_None)
  private def _scala_Some                   = _scala(_Some)
  private def _scala_xml_NamespaceBinding   = _scala_xml(_NamespaceBinding)
  private def _scala_xml_XMLUnmarshaller    = _scala_xml(_XMLUnmarshaller)

  private def apply(pos: Position, method: TermName, trees: Tree*): Tree => Tree =
    (unmarshaller: Tree) => atPos(pos)( Apply(Select(unmarshaller, method), trees.toList) )
  private def apply(pos: Position, method: TermName, text: String, texts: String*): Tree => Tree =
    apply(pos, method, const(text)+:(texts map {const(_)}) : _*)

  final def entityRef(pos: Position, name: String) = apply(pos, _skippedEntity, name) 
  final def text(pos: Position, text: String) = apply(pos, _characters, text)
  final def comment(pos: Position, text: String) = apply(pos, _comment, text)
  final def charData(pos: Position, text: String) = 
        apply(pos, _endCDATA) compose apply(pos, _characters, text) compose apply(pos, _startCDATA)
  final def procInstr(pos: Position, target: String, text: String) = apply(pos, _processingInstruction, target, text)

  final def scalaProcInstr(pos: Position, unmarshaller: Tree) =
    (_: Tree) => atPos(pos){
      Block(List(ValDef(Modifiers(Flags.LAZY), _xmlUnmarshaller, _scala_xml_XMLUnmarshaller, unmarshaller)), unmarshaller)
        // The ValDef just intends to throw an error at compile time if the unmarshaller is not such.
        // Is there a better way?
    }

  final def embeddedExpr(pos: Position, expr: Tree) = apply(pos, _embeddedExpression, expr)
  final def scalaPattern(pos: Position, pat: Tree) = apply(pos, _embeddedExpression, pat)

  /** @todo: attributes */
  final def pattern(pos: Position, n: String, children: Seq[Tree => Tree]): Tree => Tree = {
    val (prepat, labpat) = splitPrefix(n) match {
      case (Some(pre), rest)  => (const(pre), const(rest))
      case _                  => (wild, const(n))
    }
    atPos[Tree](pos) _ compose
      // apply(pos, _endElement) compose
      // compose(children) compose -- TODO: we can't ignore the children!!
      apply(pos, _elementPattern, prepat, labpat, wild)
  }

  final def parseAttribute(pos: Position, s: String): Tree => Tree = {
    val ts = xml.Utility.parseAttributeValue(s) map {
      case Text(s)      => text(pos, s)
      case EntityRef(s) => entityRef(pos, s)
    }
    atPos[Tree](pos) _ compose compose(ts.toList)
  }

  private def compose(args: Seq[Tree => Tree]): Tree => Tree = {
    args.fold(identity[Tree] _)((prev: Tree=>Tree, next: Tree=>Tree) => next compose prev)
  }

  final def unmarshallLiteral(pos: Position, args: Seq[Tree => Tree]): Tree = {
    val expr = compose(args)(apply(pos, _startDocument)(Ident(_xmlUnmarshaller)))
    expr match {
      case Block(_, _) => ValDef(NoMods, _xmlUnmarshaller, TypeTree(), expr)
          // a standalone <?scala unmarshaller?> defines value $xmlUnmarshaller
      case _ => apply(pos, _endDocument)(expr)
    }
  }

  final def unmarshallPattern(pos: Position, arg: Tree => Tree): Tree = {
    arg(apply(pos, _startDocument)(Ident(_xmlUnmarshaller)))
  }

  /** Returns (Some(prefix) | None, rest) based on position of ':' */
  private def splitPrefix(name: String): (Option[String], String) = splitWhere(name, _ == ':', true) match {
    case Some((pre, rest))  => (Some(pre), rest)
    case _                  => (None, name)
  }

  final def group(pos: Position, args: Seq[Tree => Tree]) = {
    apply(pos, _endGroup) compose atPos[Tree](pos) _ compose compose(args) compose apply(pos, _startGroup)
  }

  final def unparsed(pos: Position, str: String) = apply(pos, _unparsed, str)

  final def element(pos: Position, qname: String, attrMap: mutable.Map[String, Tree => Tree], args: Seq[Tree => Tree]): Tree => Tree = {

    def optional(name: Option[String]) = name match {
      case Some(str) => Apply(_scala_Some, List(const(str)))
      case None => _scala_None
    }

    val attributes= new ListBuffer[Tree => Tree]
    val bindings= new ListBuffer[Tree => Tree]

    def prefixMapping(name: Option[String], value: Tree => Tree) = 
      apply(pos, _endPrefixMapping) compose value compose apply(pos, _startPrefixMapping, optional(name))

    def attribute(prefix: Option[String], localName: String, value: Tree => Tree) =
      apply(pos, _endAttribute) compose value compose apply(pos, _startAttribute, optional(prefix), const(localName))
    
    for ((attrQName, attrValue) <- attrMap.iterator) splitPrefix(attrQName) match {
      case (Some("xmlns"), rest)   => bindings += prefixMapping(Some(rest), attrValue)
      case (None, "xmlns") => bindings += prefixMapping(None, attrValue)
      case (ns, localName) => attributes += attribute(ns, localName, attrValue)
    }

    val (pre, newlabel) = splitPrefix(qname)

    atPos[Tree](pos) _ compose
      apply(pos, _endElement) compose
      compose(args) compose
      compose(attributes.reverse) compose
      apply(pos, _startElement, optional(pre), const(newlabel)) compose
      compose(bindings)
  }
}