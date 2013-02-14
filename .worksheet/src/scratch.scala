object scratch {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(151); 

  def foo(f: Int => String => Boolean => Double) = {
    val stobtod = f(1)
    val btod = stobtod("some string")
    btod(true)
  };System.out.println("""foo: (f: Int => (String => (Boolean => Double)))Double""");$skip(94); 

  def bToD(b: Boolean): Double = b match {
    case true => 1.0d
    case false => 0.0d
  };System.out.println("""bToD: (b: Boolean)Double""");$skip(230); 

  def sToBToD(s: String): Boolean => Double = s match {
  	case "some string" => bToD
  	case "xyz" => throw new IllegalArgumentException("could return a different function")
  	case _ => throw new Exception("wrong string")
  };System.out.println("""sToBToD: (s: String)Boolean => Double""");$skip(26); val res$0 = 
  

  foo(i => sToBToD);System.out.println("""res0: Double = """ + $show(res$0))}

}