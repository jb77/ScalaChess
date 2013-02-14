object scratch {

  def foo(f: Int => String => Boolean => Double) = {
    val stobtod = f(1)
    val btod = stobtod("some string")
    btod(true)
  }                                               //> foo: (f: Int => (String => (Boolean => Double)))Double

  def bToD(b: Boolean): Double = b match {
    case true => 1.0d
    case false => 0.0d
  }                                               //> bToD: (b: Boolean)Double

  def sToBToD(s: String): Boolean => Double = s match {
  	case "some string" => bToD
  	case "xyz" => throw new IllegalArgumentException("could return a different function")
  	case _ => throw new Exception("wrong string")
  }                                               //> sToBToD: (s: String)Boolean => Double
  

  foo(i => sToBToD)                               //> res0: Double = 1.0

}