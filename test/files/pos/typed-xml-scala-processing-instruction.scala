class Test {
  def f1 = {
    <?scala {""}?><more><xml/></more>
    ;
    <xml/>
  }
  def f2= {
    <embedded><before/><?scala  {null}  ?><after/></embedded>
    ;
    <xml/>
  }
  def f3= {
    <?scala {""}?>
    ;
    <xml/>
  }
}
