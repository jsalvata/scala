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
    val _XMLUnmarshaller:       NameType = "XMLUnmarshaller"
  }

  private trait XMLTermNames extends TermNames {
    val _None:          NameType = "None"
    val _Some:          NameType = "Some"

    val _xml:           NameType = "xml"

    val _xmlUnmarshaller: NameType = "$xmlUnmarshaller"

    // The following are named after Scala/XML grammar non-terminals:
    val _startXmlExpr:  NameType = "startXmlExpr"
    val _endXmlExpr:    NameType = "endXmlExpr"
    val _sTag:          NameType = "sTag" // also called for sTagP
    val _eTag:          NameType = "eTag" // also called for eTagP
    val _charData:      NameType = "charData" // also called for CData
    val _comment:       NameType = "comment"
    val _cdStart:       NameType = "cdStart"
    val _cdEnd:         NameType = "cdEnd"
    val _pi:            NameType = "pi"
    val _entityRef:     NameType = "entityRef"
    val _scalaExpr:     NameType = "scalaExpr"
    val _scalaPatterns: NameType = "scalaPatterns"

    val _startXmlPattern: NameType = "startXmlPattern"
    val _endXmlPattern: NameType = "endXmlPattern"

    val _startAttribute:NameType = "startAttribute"
    val _endAttribute:  NameType = "endAttribute"
  }

  private object xmltypes extends XMLTypeNames {
    type NameType = TypeName
    implicit def createNameType(name: String): TypeName = newTypeName(name)
  }
  private object xmlterms extends XMLTermNames {
    type NameType = TermName
    implicit def createNameType(name: String): TermName = newTermName(name)
  }

  import xmltypes.{_XMLUnmarshaller}
  
  import xmlterms.{_None, _Some, _xml, _xmlUnmarshaller,
    _startXmlExpr, _endXmlExpr, _sTag, _eTag,
    _charData, _comment, _cdStart, _cdEnd, _pi,
    _entityRef, _scalaExpr, _scalaPatterns,
    _startXmlPattern, _endXmlPattern,
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
  private def _scala_xml_XMLUnmarshaller    = _scala_xml(_XMLUnmarshaller)

  private def apply(pos: Position, method: TermName, trees: Tree*): Tree => Tree =
    (unmarshaller: Tree) => atPos(pos)( Apply(Select(unmarshaller, method), trees.toList) )
  private def apply(pos: Position, method: TermName, text: String, texts: String*): Tree => Tree =
    apply(pos, method, const(text)+:(texts map {const(_)}) : _*)

  final def entityRef(pos: Position, name: String) = apply(pos, _entityRef, name) 
  final def text(pos: Position, text: String) = apply(pos, _charData, text)
  final def comment(pos: Position, text: String) = apply(pos, _comment, text)
  final def charData(pos: Position, text: String) = 
        apply(pos, _cdEnd) compose apply(pos, _charData, text) compose apply(pos, _cdStart)
  final def procInstr(pos: Position, target: String, text: String) = apply(pos, _pi, target, text)

  final def scalaProcInstr(pos: Position, unmarshaller: Tree) = {
    apply(pos, _startXmlExpr) compose (
    (_: Tree) => atPos(pos){
      Block(List(ValDef(Modifiers(Flags.LAZY), _xmlUnmarshaller, _scala_xml_XMLUnmarshaller, unmarshaller)), 
          unmarshaller)
        // The ValDef just intends to throw an error at compile time if the unmarshaller is not such.
        // Is there a better way?
    })
  }

  final def embeddedExpr(pos: Position, expr: Tree) = apply(pos, _scalaExpr, expr)

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
    val expr = compose(args)(apply(pos, _startXmlExpr)(Ident(_xmlUnmarshaller)))
    expr match {
      case Apply(Select(unmarshaller @ Block(_, _),_startXmlExpr), _) => 
        ValDef(NoMods, _xmlUnmarshaller, TypeTree(), unmarshaller)
          // a standalone <?scala unmarshaller?> defines value $xmlUnmarshaller -- really ugly TODO: improve
      case _ => apply(pos, _endXmlExpr)(expr)
    }
  }

  final def unparsed(pos: Position, str: String) =
    apply(pos, _eTag) compose apply(pos, _charData, str) compose apply(pos, _sTag, "xml:unparsed")

  final def element(pos: Position, 
                    qname: String, 
                    attrMap: mutable.Map[String, Tree => Tree], 
                    children: Seq[Tree => Tree]): Tree => Tree = {

    val attributes = for ((name, value) <- attrMap.toSeq) yield
      apply(pos, _endAttribute) compose value compose apply(pos, _startAttribute, name)

    apply(pos, _eTag) compose
      compose(children) compose
      compose(attributes) compose
      apply(pos, _sTag, const(qname))
  }

  final def scalaPatterns(pos: Position, expr: Tree) = apply(pos, _scalaPatterns, expr)

  final def pattern(pos: Position,
                    qName: String,
                    attrMap: mutable.Map[String, Tree => Tree], 
                    children: Seq[Tree => Tree]): Tree => Tree = 
    element(pos, qName, attrMap, children)

  final def unmarshallPattern(pos: Position, arg: Tree => Tree): Tree = {
    Ident(_xmlUnmarshaller)
    //apply(pos, _endXmlPattern)( arg (apply(pos, _startXmlPattern)(Ident(_xmlUnmarshaller))))
  }
}