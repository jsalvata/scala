import scala.xml._

object Test {
  def main(a: Array[String]) {
    // Each section contains 1-4 tests:
    // 1.- Check the unapplySeq method (validates ScalaXMLUnmarshaller)
    // 2.- Check that it can be used as a pattern (validates this test itself)
    // 3.- Check whether pattern without variable bindings matches 
    // 4.- If appropriate, check that the patter with variable binding extracts values
    {
      val P= $xmlUnmarshaller.startXmlPattern().sTag_a().eTag().endXmlPattern

      {
        val xml= <a/>
        assert(P.unapplySeq(xml) == Some(Nil))
        assert(xml match { case P() => true })
        assert(xml match { case <a/> => true })
      }

      {
        val xml= <b/>
	assert(P.unapplySeq(xml) == None)
	assert(xml match { case P() => false; case _ => true })
	assert(xml match { case <a/> => false; case _ => true })
      }
    }
    {
      val P= $xmlUnmarshaller.startXmlPattern().sTag_a().sTag_b().eTag().sTag_c().eTag().eTag().endXmlPattern

      {
	val xml= <a><b/><c/></a>
	assert(P.unapplySeq(xml) == Some(Nil))
	assert(xml match { case P() => true })
	assert(xml match { case <a><b/><c/></a> => true })
      }

      {
	val xml= <a><b/></a>
	assert(P.unapplySeq(xml) == None)
	assert(xml match { case P() => false; case _ => true })
	assert(xml match { case <a><b/><c/></a> => false; case _ => true })
      }
    }
    {
      val P= $xmlUnmarshaller.startXmlPattern.sTag_a().scalaPattern().eTag().endXmlPattern

      {
        val xml= <a><b/></a>
	assert(P.unapplySeq(xml) == Some(List(<b/>)))
	assert((xml match { case P(b) => b }) == <b/>)
	assert(xml match { case <a>{_}</a> => true })
	assert((xml match { case <a>{b}</a> => b }) == <b/>)
      }

      {
        val xml= <a><b><c/></b></a>
	assert(P.unapplySeq(xml) == Some(List(<b><c/></b>)))
	assert((xml match { case P(b) => b }) == <b><c/></b>)
	assert(xml match { case <a>{_}</a> => true })
	assert((xml match { case <a>{b}</a> => b }) == <b><c/></b>)
      }

      {
	val xml= <a><b/><c/></a>
	assert(P.unapplySeq(xml) == None)
	assert(xml match { case P(_) => false; case _ => true })
	assert(xml match { case <a>{_}</a> => false; case _ => true })
      }
    }
    {
      val P= $xmlUnmarshaller.startXmlPattern.sTag_a().sTag_i().eTag().scalaStarPattern().eTag().endXmlPattern

      {
	val xml= <a><i/><b/></a>
	assert(P.unapplySeq(xml) == Some(List(List(<b/>))))
	assert((xml match { case P(b) => b }) ==  List(<b/>))
	assert(xml match { case <a><i/>{ _* }</a> => true })
	assert((xml match { case <a><i/>{ b @ _* }</a> => b }) ==  List(<b/>))
      }

      {
        val xml= <a><i/><b><c/></b></a>
	assert(P.unapplySeq(xml) == Some(List(List(<b><c/></b>))))
	assert((xml match { case P(b) => b }) == List(<b><c/></b>))
	assert(xml match { case <a><i/>{ _* }</a> => true })
	assert((xml match { case <a><i/>{ b @ _* }</a> => b }) == List(<b><c/></b>))
      }

      {
        val xml= <a><i/><b/>some text<c/><d/></a>
        assert(P.unapplySeq(xml) == Some(List(List(<b/>, Text("some text"), <c/>, <d/>))))
	assert((xml match { case P(b) => b }) == List(<b/>, Text("some text"), <c/>, <d/>))
	assert(xml match { case <a><i/>{ _* }</a> => true })
	assert((xml match { case <a><i/>{ b @ _* }</a> => b }) == List(<b/>, Text("some text"), <c/>, <d/>))
      }
    }
    {
      val P= $xmlUnmarshaller.startXmlPattern.sTag_a().charData("hello").scalaStarPattern().eTag().endXmlPattern

      {
        val xml= <a>hello<b/></a>
        assert(P.unapplySeq(<a>hello<b/></a>) == Some(List(List(<b/>))))
	assert((xml match { case P(b) => b }) == List(<b/>))
	assert(xml match { case <a>hello{ _* }</a> => true })
	assert((xml match { case <a>hello{ b @ _* }</a> => b }) == List(<b/>))
      }

      {
        val xml= <a>hello<b><c/></b></a>
        assert(P.unapplySeq(<a>hello<b><c/></b></a>) == Some(List(List(<b><c/></b>))))
	assert((xml match { case P(b) => b }) == List(<b><c/></b>))
	assert(xml match { case <a>hello{ _* }</a> => true })
	assert((xml match { case <a>hello{ b @ _* }</a> => b }) == List(<b><c/></b>))
      }
      {
	val xml= <a>hello<b/>some text<c/><d/></a>
        assert(P.unapplySeq(xml) == Some(List(List(<b/>, Text("some text"), <c/>, <d/>))))
	assert((xml match { case P(b) => b }) == List(<b/>, Text("some text"), <c/>, <d/>))
	assert(xml match { case <a>hello{ _* }</a> => true })
	assert((xml match { case <a>hello{ b @ _* }</a> => b }) == List(<b/>, Text("some text"), <c/>, <d/>))
      }
      {
	val xml= <a>goodbye<b/>some text<c/><d/></a>
        assert(P.unapplySeq(xml) == None)
        assert(xml match { case P() => false; case _ => true })
	assert(xml match { case <a>hello{ _* }</a> => false; case _ => true})
      }
    }
    {
      val P= $xmlUnmarshaller.startXmlPattern.sTag_a().scalaPattern().scalaStarPattern().eTag().endXmlPattern

      {
	val xml = <a>hello<b/></a>
	assert(P.unapplySeq(xml) == Some(List(Text("hello"), List(<b/>))))
	assert((xml match { case P(b, c) => (b, c) }) == (Text("hello"), List(<b/>)))
	assert(xml match { case <a>{_}{_*}</a> => true })
	assert(xml match { case <a>{_, _*}</a> => true })
	assert((xml match { case <a>{b}{c@_*}</a> => (b, c) }) == (Text("hello"), List(<b/>)))
	assert((xml match { case <a>{b, c@_*}</a> => (b, c) }) == (Text("hello"), List(<b/>)))
      }
      {
        val xml = <a>hello<b><c/></b></a>
        assert(P.unapplySeq(xml) == Some(List(Text("hello"), List(<b><c/></b>))))
	assert((xml match { case P(b, c) => (b, c) }) == (Text("hello"), List(<b><c/></b>)))
	assert(xml match { case <a>{_}{_*}</a> => true })
	assert(xml match { case <a>{_, _*}</a> => true })
	assert((xml match { case <a>{b}{c@_*}</a> => (b, c) }) == (Text("hello"), List(<b><c/></b>)))
	assert((xml match { case <a>{b, c@_*}</a> => (b, c) }) == (Text("hello"), List(<b><c/></b>)))
      }
      {
        val xml = <a>hello<b/>some text<c/><d/></a>
        assert(P.unapplySeq(<a>hello<b/>some text<c/><d/></a>) == Some(List(Text("hello"), List(<b/>, Text("some text"), <c/>, <d/>))))
	assert((xml match { case P(b, c) => (b, c) }) == (Text("hello"), List(<b/>, Text("some text"), <c/>, <d/>)))
	assert((xml match { case <a>{b}{c@_*}</a> => (b, c) }) == (Text("hello"), List(<b/>, Text("some text"), <c/>, <d/>)))
	assert((xml match { case <a>{b, c@_*}</a> => (b, c) }) == (Text("hello"), List(<b/>, Text("some text"), <c/>, <d/>)))
	assert(xml match { case <a>{_}{_*}</a> => true })
	assert(xml match { case <a>{_, _*}</a> => true })
      }
    }
  }
}
