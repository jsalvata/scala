import scala.xml._

object Test {

  def main(a: Array[String]) {
    val X= $xmlUnmarshaller.startXmlPattern().sTag_a().eTag().endXmlPattern

    assert(X.unapplySeq(<a/>) == Some(Nil))
    assert(X.unapplySeq(<b/>) == None)

    val Y= $xmlUnmarshaller.startXmlPattern().sTag_a().sTag_b().eTag().sTag_c().eTag().eTag().endXmlPattern

    assert(Y.unapplySeq(<a><b/><c/></a>) == Some(Nil))
    assert(Y.unapplySeq(<a><b/></a>) == None)

    val Z= $xmlUnmarshaller.startXmlPattern.sTag_a().scalaPattern().eTag().endXmlPattern

    assert(Z.unapplySeq(<a><b/></a>) == Some(List(<b/>)))
    assert(Z.unapplySeq(<a><b><c/></b></a>) == Some(List(<b><c/></b>)))
    assert(Z.unapplySeq(<a><b/><c/></a>) == None)

    val T= $xmlUnmarshaller.startXmlPattern.sTag_a().sTag_i().eTag().scalaStarPattern().eTag().endXmlPattern

    assert(T.unapplySeq(<a><i/><b/></a>) == Some(List(List(<b/>))))
    assert(T.unapplySeq(<a><i/><b><c/></b></a>) == Some(List(List(<b><c/></b>))))
    assert(T.unapplySeq(<a><i/><b/>some text<c/><d/></a>) == Some(List(List(<b/>, Text("some text"), <c/>, <d/>))))

    val U= $xmlUnmarshaller.startXmlPattern.sTag_a().charData("hello").scalaStarPattern().eTag().endXmlPattern

    assert(U.unapplySeq(<a>hello<b/></a>) == Some(List(List(<b/>))))
    assert(U.unapplySeq(<a>hello<b><c/></b></a>) == Some(List(List(<b><c/></b>))))
    assert(U.unapplySeq(<a>hello<b/>some text<c/><d/></a>) == Some(List(List(<b/>, Text("some text"), <c/>, <d/>))))
    assert(U.unapplySeq(<a>goodbye<b/>some text<c/><d/></a>) == None)

    val V= $xmlUnmarshaller.startXmlPattern.sTag_a().scalaPattern().scalaStarPattern().eTag().endXmlPattern

    assert(V.unapplySeq(<a>hello<b/></a>) == Some(List(Text("hello"), List(<b/>))))
    assert(V.unapplySeq(<a>hello<b><c/></b></a>) == Some(List(Text("hello"), List(<b><c/></b>))))
    assert(V.unapplySeq(<a>hello<b/>some text<c/><d/></a>) == Some(List(Text("hello"), List(<b/>, Text("some text"), <c/>, <d/>))))
  }
}
