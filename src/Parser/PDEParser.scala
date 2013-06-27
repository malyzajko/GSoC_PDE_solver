package Parser
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.JavaTokenParsers
import token._


object PDEParser extends StandardTokenParsers  {
  
	lexical.delimiters ++= List( "(", ")", "=", "+", "-", "*", "/", "\n" )
	lexical.reserved ++= List(
	    "sin", "cos", "tan", "ln", "log", "exp", "sqrt", 
	    "_{xx}", "_{tt}", "_{tx}", "_{xt}", "_x", "_t")
	    
	    

}