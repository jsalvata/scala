import scala.xml.SymbolicXMLUnmarshaller

class Test {
  def f1 = {
    <?scala { new SymbolicXMLUnmarshaller() }?><more><xml/></more>
    ;
    <xml/>
  }
  def f2= {
    <embedded><before/><?scala  {null}  ?><after/></embedded>
    ;
    <xml/>
  }
  def f3= {
    <?scala { new SymbolicXMLUnmarshaller() }?>
    ;
    <xml/>
  }
}
