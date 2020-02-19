package cnv

object MyMath {
  /** Mean value of collection */
  def mean(xs: Iterable[Int]) = {
    xs match {
      case Nil => 0.0
      case xs => xs.reduce(_ + _) / xs.size.toDouble
    }
  }

  /** Uncorrected sample standard deviation */
  def usstddef(xs: Iterable[Int]) = {
    val xb = mean(xs)
    xs match {
      case Nil => 0.0
      case xs => math.sqrt(xs.map(x => math.pow(x - xb, 2)).reduce(_ + _) / xs.size.toDouble)
    }
  }
}
