package pde.variable;

    import pde.expression._
    import pde.expression._

object Variable {    
    implicit def toBFunction(a: Tuple2[BFint2, LEQexp]) = BFunction(a._1.u, a._1.const, a._1.x, a._1.exp, a._2)
	private var variables: List[String] = List.empty
	
	def is_unlocked(xname: String) = !variables.contains(xname)
	
	def lock(name: String){
	  variables = name::variables
	}
    
	
}
    class invalidVariableException(x: Variable) extends RuntimeException

    import Variable._
    import pde.expression.Expression.double2Const
    
    abstract class Variable extends Expr {
    	def apply(x: String) = new NFVariable("x")
      def unnaply(x: NFVariable) = x
  	}
	
    trait Function
    
	case class NFVariable(xname: String) extends Variable{
  	  assert (is_unlocked(xname))
	  lock(xname)
	  
  	  val x = Symbol(xname)

  	  
  	  def -(t: NFVariable) = Sub(this, t)
  	  def infix_-(c: Double) = Sub(c, this)  
  	  def infix_<(c: Double) = LEQ(c, this)
  	  
	}
	
	case class FVariable(uname: String, vars: NFVariable*) extends Variable with Function {
	  assert(is_unlocked(uname))
	  lock(uname)
	  
	  val u = Symbol(uname)
	  
	  def apply(const: Double, x: NFVariable) = {
	    assert(vars.contains(x))
	    new BFint(this, const, x)
	    
	  }
	  
	  def apply(x: NFVariable, const: Double) = {
	    assert(vars.contains(x))
	    new BFint(this, const, x)
	  }
	}
	
	case class Const(c: Double) extends Variable{
	  def *(k: Double) = Const(c*k)
	  def /(k: Double) = Const(c/k)
	  def +(k: Double) = Const(c+k)
	  def -(k: Double) = Const(c-k)
	  def <(x: NFVariable) = LEQ(c, x)
	}
	
	class BFint(u: FVariable, const: Double, x: NFVariable) {	  
	  def ===(exp: Expr) = new BFint2(u, const, x, exp)	  
	}
	case class BFint2(u: FVariable, const: Double, x: NFVariable, exp: Expr)
	
	case class BFunction(function: FVariable, const: Double, x: NFVariable, exp: Expr, bounds: LEQexp) {
	  assert((bounds.variable)==x)
	  
	  private def position(y: Variable, xs: List[NFVariable]): Int = xs match {
	    case Nil => 10000 //TODO throw exception
	    case l::ls if l == y => 0
	    case l::ls => 1 + position(y, ls)
	  }
	  
	  def lowerPoint: (Double, Double) = if (position(x, function.vars.toList)==0) (bounds.lower, const) 
			  		   else (const, bounds.lower) 
      
      def upperPoint: (Double, Double) = if (position(x, function.vars.toList)==0) (bounds.upper, const) 
			  		   else  (const, bounds.upper)
			  		   
	  def lowerValue = if (position(x, function.vars.toList)==0) exp.eval(Map(x -> bounds.lower, function.vars(1) -> const))
	  				   else exp.eval(Map(function.vars(1) -> bounds.lower,x -> const))
	  				   
	  def upperValue = if (position(x, function.vars.toList)==0) exp.eval(Map(x -> bounds.upper, function.vars(1) -> const))
	  				   else exp.eval(Map(function.vars(1) -> bounds.upper,x -> const))				   
	  
	}
	
