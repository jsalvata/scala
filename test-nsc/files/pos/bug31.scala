object Main {

    trait Ensure[a] {
        def ensure(postcondition: a => Boolean): a
    }

    def require[a](precondition: => Boolean)(command: => a): Ensure[a] = 
        if (precondition)
            new Ensure[a] {
	        def ensure(postcondition: a => Boolean): a = {
	            val result = command;
	            if (postcondition(result)) result
	            else error("Assertion error")
                }
	    }
        else
            error("Assertion error");

    def arb[a](s: List[a]) =
        require (! s.isEmpty) {
           s.head
        } ensure (result => s contains result);

    def main(args: Array[String]) = {
    	val s = List(1, 2);
        System.out.println(arb(s))
    }

}
