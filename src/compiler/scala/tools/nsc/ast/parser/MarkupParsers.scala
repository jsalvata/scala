/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Burak Emir
 */

package scala.tools.nsc
package ast.parser

import scala.collection.mutable
import mutable.{ Buffer, ArrayBuffer, ListBuffer, Map }
import scala.util.control.ControlThrowable
import scala.tools.nsc.util.{SourceFile,CharArrayReader}
import scala.xml.{ Text, TextBuffer }
import scala.xml.parsing.MarkupParserCommon
import scala.xml.Utility.{ isNameStart, isNameChar, isSpace }
import scala.reflect.internal.Chars.{ SU, LF }

// XXX/Note: many/most of the functions in here are almost direct cut and pastes
// from another file - scala.xml.parsing.MarkupParser, it looks like.
// (It was like that when I got here.) They used to be commented "[Duplicate]" but
// since approximately all of them were, I snipped it as noise.  As far as I can
// tell this wasn't for any particularly good reason, but slightly different
// compiler and library parser interfaces meant it would take some setup.
//
// I rewrote most of these, but not as yet the library versions: so if you are
// tempted to touch any of these, please be aware of that situation and try not
// to let it get any worse.  -- paulp

/** This trait ...
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
trait MarkupParsers {
  self: Parsers =>

  case object MissingEndTagControl extends ControlThrowable {
    override def getMessage = "start tag was here: "
  }

  case object ConfusedAboutBracesControl extends ControlThrowable {
    override def getMessage = " I encountered a '}' where I didn't expect one, maybe this tag isn't closed <"
  }

  case object TruncatedXMLControl extends ControlThrowable {
    override def getMessage = "input ended while parsing XML"
  }

  import global._

  class MarkupParser(parser: SourceFileParser, final val preserveWS: Boolean) extends MarkupParserCommon {

    import Tokens.{ EMPTY, LBRACE, RBRACE }
    
    type PositionType = Position
    type InputType    = CharArrayReader
    type ElementType  = Tree => Tree
    type AttributesType = mutable.Map[String, Tree => Tree]
    type NamespaceType = Any  // namespaces ignored

    def mkAttributes(name: String, other: NamespaceType): AttributesType = xAttributes

    val eof = false

    def truncatedError(msg: String): Nothing = throw TruncatedXMLControl
    def xHandleError(that: Char, msg: String) =
      if (ch == SU) throw TruncatedXMLControl
      else reportSyntaxError(msg)

    var input : CharArrayReader = _
    def lookahead(): BufferedIterator[Char] =
      (input.buf drop input.charOffset).iterator.buffered

    import parser.{ symbXMLBuilder => handle, o2p, r2p }

    def curOffset : Int = input.charOffset - 1
    var tmppos : Position = NoPosition
    def ch = input.ch
    /** this method assign the next character to ch and advances in input */
    def nextch() { input.nextChar() }

    protected def ch_returning_nextch: Char = {
      val result = ch; input.nextChar(); result
    }
    
    def mkProcInstr(position: Position, name: String, text: String): ElementType =
      parser.symbXMLBuilder.procInstr(position, name, text)

    var xEmbeddedBlock = false

    private var debugLastStartElement = new mutable.Stack[(Int, String)]
    private def debugLastPos = debugLastStartElement.top._1
    private def debugLastElem = debugLastStartElement.top._2

    private def errorBraces() = {
      reportSyntaxError("in XML content, please use '}}' to express '}'")
      throw ConfusedAboutBracesControl
    }
    def errorNoEnd(tag: String) = {
      reportSyntaxError("expected closing tag of " + tag)
      throw MissingEndTagControl
    }

    /** checks whether next character starts a Scala block, if yes, skip it.
     * @return true if next character starts a scala block 
     */
    def xCheckEmbeddedBlock: Boolean = {
      // attentions, side-effect, used in xText
      xEmbeddedBlock = (ch == '{') && { nextch; (ch != '{') }
      xEmbeddedBlock
    }

    /** parse attribute and add it to listmap
     *  [41] Attributes   ::= { S Name Eq AttValue }
     *       AttValue     ::= `'` { _  } `'`
     *                      | `"` { _ } `"`
     *                      | `{` scalablock `}`
     */
    def xAttributes = {
      val aMap = mutable.HashMap[String, Tree => Tree]()

      while (isNameStart(ch)) {
        val start = curOffset
        val key = xName
        xEQ
        val delim = ch
        val mid = curOffset
        val value: Tree => Tree = ch match {
          case '"' | '\'' =>
            val tmp = xAttributeValue(ch_returning_nextch)
            
            try handle.parseAttribute(r2p(start, mid, curOffset), tmp)
            catch {
              case e: RuntimeException => 
                errorAndResult("error parsing attribute value", (_:Tree) => parser.errorTermTree)
            }
          
          case '{'  =>
            nextch
            xEmbeddedExpr
          case SU =>
            throw TruncatedXMLControl
          case _ =>
            errorAndResult("' or \" delimited attribute value or '{' scala-expr '}' expected", (_: Tree) => Literal(Constant("<syntax-error>")))
        }
        // well-formedness constraint: unique attribute names
        if (aMap contains key)
          reportSyntaxError("attribute %s may only be defined once" format key)

        aMap(key) = value
        if (ch != '/' && ch != '>')
          xSpace
      }
      aMap
    }

    /** '<! CharData ::= [CDATA[ ( {char} - {char}"]]>"{char} ) ']]>'
     *
     * see [15]
     */
    def xCharData: ElementType = {
      val start = curOffset
      xToken("[CDATA[")
      val mid = curOffset
      xTakeUntil(handle.charData, () => r2p(start, mid, curOffset), "]]>")
    }

    def xUnparsed: ElementType = {
      val start = curOffset
      xTakeUntil(handle.unparsed, () => r2p(start, start, curOffset), "</xml:unparsed>")
    }

    /** Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
     *
     * see [15]
     */
    def xComment: ElementType = {
      val start = curOffset - 2   // Rewinding to include "<!"
      xToken("--")
      xTakeUntil(handle.comment, () => r2p(start, start, curOffset), "-->")
    }

    def appendText(pos: Position, ts: Buffer[ElementType], txt: String) {
      val toAppend =
        if (preserveWS) Seq(txt)
        else TextBuffer.fromString(txt).toText map (_.text)
      
      toAppend foreach (t => ts append handle.text(pos, t))
    }

    /** adds entity/character to ts as side-effect 
     *  @precond ch == '&'
     */
    def content_AMP(ts: ArrayBuffer[ElementType]) {
      nextch
      val toAppend = ch match {
        case '#' => // CharacterRef
          nextch
          val theChar = handle.text(tmppos, xCharRef)
          xToken(';')
          theChar
        case _ =>   // EntityRef
          val n = xName
          xToken(';')
          handle.entityRef(tmppos, n)
      }

      ts append toAppend
    }

    /**
     *  @precond ch == '{'
     *  @postcond: xEmbeddedBlock == false!
     */
    def content_BRACE(p: Position, ts: ArrayBuffer[ElementType]): Unit =
      if (xCheckEmbeddedBlock) ts append xEmbeddedExpr
      else appendText(p, ts, xText)

    /** Returns true if it encounters an end tag (without consuming it),
     *  appends trees to ts as side-effect.
     *
     *  @param ts ...
     *  @param atStartOfLiteral true iif this is the initial XmlContent in an XmlExpr (see ScalaReference 10.1)
     *  @return   ...
     */
    private def content_LT(ts: ArrayBuffer[ElementType], atStartOfLiteral: Boolean): Boolean = {
      if (ch == '/')
        return true   // end tag

      val toAppend = ch match {
        case '!'    => nextch ; if (ch =='[') xCharData else xComment // CDATA or Comment
        case '?'    => nextch ; xProcInstr(atStartOfLiteral)          // PI
        case _      => element                                        // child node
      }

      ts append toAppend
      false
    }

    /** TODO: to keep the existing design pattern, this method should be pushed down
     *  from MarkupParserCommon, instead of overridden here?
     * 
     *  TODO: the required syntax for scala PIs is <?scala {block}?>, where the block
     *  evaluates to the desired unmarshaller. Think out better syntax.
     * 
     *  @param atStartOfLiteral true iif this is the initial XmlContent in an XmlExpr (see ScalaReference 10.1)
     */
    def xProcInstr(atStartOfLiteral: Boolean): ElementType = {
      val n = xName
      if (n == "scala") {
        // TODO: bug: XML syntax requires either a space or the end of the PI here.
        //   but I don't want to break backward compatibility (yet), so I'm only requiring
        //   it for scala PIs
        xSpace
        xSpaceOpt
        if (!atStartOfLiteral)
          reportSyntaxError("scala processing instructions can only appear at start of XML literal")
        else if (ch != '{') 
          reportSyntaxError("expected start of Scala block")
        nextch
        val block= escapeToScala(parser.block(), "block")
        val res= handle.scalaProcInstr(tmppos, block)
        xSpaceOpt
        if (ch != '?' || { nextch; ch } != '>')
          reportSyntaxError("expected end of processing instruction")
        nextch
        res
      }
      else {
        xSpaceOpt
        xTakeUntil(handle.procInstr(_, n, _), () => tmppos, "?>")
      }
    }

    def content: Buffer[ElementType] = {
      val ts = new ArrayBuffer[ElementType]
      while (true) {
        if (xEmbeddedBlock)
          ts append xEmbeddedExpr
        else {
          tmppos = o2p(curOffset)
          ch match {
            // end tag, cdata, comment, pi or child node
            case '<'  => nextch ; if (content_LT(ts, false)) return ts
            // either the character '{' or an embedded scala block }
            case '{'  => content_BRACE(tmppos, ts)  // }
            // EntityRef or CharRef 
            case '&'  => content_AMP(ts)
            case SU   => return ts
            // text content - here xEmbeddedBlock might be true
            case _    => appendText(tmppos, ts, xText)
          }
        }
      }
      unreachable
    }

    /** '<' element ::= xmlTag1 '>'  { xmlExpr | '{' simpleExpr '}' } ETag
     *                | xmlTag1 '/' '>'
     */
    def element: ElementType = {
      val start = curOffset
      val (qname, attrMap) = xTag(())
      if (ch == '/') { // empty element
        xToken("/>")
        handle.element(r2p(start, start, curOffset), qname, attrMap, new ListBuffer[ElementType])
      }
      else { // handle content
        xToken('>')
        if (qname == "xml:unparsed")
          return xUnparsed

        debugLastStartElement.push((start, qname))
        val ts = content
        xEndTag(qname)
        debugLastStartElement.pop
        val pos = r2p(start, start, curOffset)
        handle.element(pos, qname, attrMap, ts)
      }
    }

    /** parse character data.
     *  precondition: xEmbeddedBlock == false (we are not in a scala block)
     */
    private def xText: String = {
      assert(!xEmbeddedBlock, "internal error: encountered embedded block")
      val buf = new StringBuilder
      def done = buf.toString

      while (ch != SU) {
        if (ch == '}') {
          if (charComingAfter(nextch) == '}') nextch
          else errorBraces()
        }

        buf append ch
        nextch
        if (xCheckEmbeddedBlock || ch == '<' ||  ch == '&')
          return done
      }
      done
    }

    /** Some try/catch/finally logic used by xLiteral and xLiteralPattern.  */
    private def xLiteralCommon(f: () => Tree, ifTruncated: String => Unit): Tree = {
      try return f()
      catch {
        case c @ TruncatedXMLControl  =>
          ifTruncated(c.getMessage)
        case c @ (MissingEndTagControl | ConfusedAboutBracesControl) =>
          parser.syntaxError(debugLastPos, c.getMessage + debugLastElem + ">")
        case _: ArrayIndexOutOfBoundsException =>
          parser.syntaxError(debugLastPos, "missing end tag in XML literal for <%s>" format debugLastElem)
      }
      finally parser.in resume Tokens.XMLSTART
      
      parser.errorTermTree
    }

    /** Use a lookahead parser to run speculative body, and return the first char afterward. */
    private def charComingAfter(body: => Unit): Char = {
      try {
        input = input.lookaheadReader
        body
        ch
      }
      finally input = parser.in
    }

    /** xLiteral = element { element }
     *  @return Scala representation of this xml literal
     */
    def xLiteral: Tree = xLiteralCommon(
      () => {
        input = parser.in
        handle.isPattern = false
      
        val ts = new ArrayBuffer[ElementType]
        val start = curOffset
        tmppos = o2p(curOffset)    // Iuli: added this line, as it seems content_LT uses tmppos when creating trees
        content_LT(ts, true)
      
        // parse more XML ?        
        if (charComingAfter(xSpaceOpt) == '<') {
          xSpaceOpt
          while (ch == '<') {
            nextch
            ts append element
            xSpaceOpt
          }
        }
        handle.unmarshallLiteral(r2p(start, start, curOffset), ts)
      },
      msg => parser.incompleteInputError(msg)
    )

    /** @see xmlPattern. resynchronizes after successful parse 
     *  @return this xml pattern
     */
    def xLiteralPattern: Tree = xLiteralCommon(
      () => {
        input = parser.in
        saving[Boolean, Tree](handle.isPattern, handle.isPattern = _) {
          val start= curOffset
          handle.isPattern = true
          val tree = xPattern
          xSpaceOpt
          handle.unmarshallPattern(r2p(start, start, curOffset), tree)
        }
      },
      msg => parser.syntaxError(curOffset, msg)
    )

    def escapeToScala[A](op: => A, kind: String) = {
      xEmbeddedBlock = false
      val res = saving[List[Int], A](parser.in.sepRegions, parser.in.sepRegions = _) {
        parser.in resume LBRACE
        op
      }
      if (parser.in.token != RBRACE)
        reportSyntaxError(" expected end of Scala "+kind)
      
      res
    }

    def xEmbeddedExpr: ElementType = {
      val start= curOffset;
      val block= escapeToScala(parser.block(), "block")
      handle.embeddedExpr(r2p(start, start, curOffset), block)
    }

    /** xScalaPatterns  ::= patterns
     */
    def xScalaPatterns: List[ElementType] = {
      val start= curOffset;
      val patterns= escapeToScala(parser.seqPatterns(), "pattern")
      patterns.map(handle.scalaPattern(r2p(start, start, curOffset), _))
    }

    def reportSyntaxError(pos: Int, str: String) = parser.syntaxError(pos, str)
    def reportSyntaxError(str: String) {
      reportSyntaxError(curOffset, "in XML literal: " + str)
      nextch()
    }

    /** '<' xPattern  ::= Name [S] { xmlPattern | '{' pattern3 '}' } ETag
     *                  | Name [S] '/' '>'
     */
    def xPattern: ElementType = {      
      var start = curOffset
      val qname = xName
      debugLastStartElement.push((start, qname))
      xSpaceOpt

      val ts = new ArrayBuffer[ElementType]
      val isEmptyTag = (ch == '/') && { nextch ; true }
      xToken('>')

      if (!isEmptyTag) {        
        // recurses until it hits a termination condition, then returns
        def doPattern: Boolean = {
          val start1 = curOffset
          if (xEmbeddedBlock) ts ++= xScalaPatterns
          else ch match {
            case '<'  => // tag
              nextch
              if (ch != '/') ts append xPattern   // child
              else return false                   // terminate
              
            case '{'  => // embedded Scala patterns
              while (ch == '{') { // This loop is a bug: https://issues.scala-lang.org/browse/SI-5154. Unfortunately removing the loop won't be enough to fix it, so not touching for the time being. 
                nextch
                ts ++= xScalaPatterns
              }
              assert(!xEmbeddedBlock, "problem with embedded block")
              
            case SU   =>
              throw TruncatedXMLControl
              
            case _    => // text
              appendText(r2p(start1, start1, curOffset), ts, xText)
              // here xEmbeddedBlock might be true:
              // if (xEmbeddedBlock) throw new ApplicationError("after:"+text); // assert
          }
          true
        }
        
        while (doPattern) { }  // call until false
        xEndTag(qname)
        debugLastStartElement.pop
      }
      
      handle.pattern(r2p(start, start, curOffset), qname, Map.empty, ts)
    }
  } /* class MarkupParser */
}
