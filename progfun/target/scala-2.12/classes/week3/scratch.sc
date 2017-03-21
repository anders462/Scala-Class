import week3.{Rational}

object scratch {
  new Rational(1,2)

  def error(msg: String) = throw new Error(msg)

  error("hello")
}