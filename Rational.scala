case class Rational(n: Int, d: Int = 1) extends Ordered[Rational | Int]:

  // The denom should't be 0
  require(d != 0)

  // Function for reducing the Fraction/Rational
  private def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)
  private val gcd: Int = gcd(n.abs, d.abs)
  
  // Fields that will be reduced after passing the arguments
  val (nuber, denom) = 
    val (nuber, denom) = (n / gcd, d / gcd)
    if nuber < 0 && denom < 0 then (-nuber, -denom) else (nuber, denom)

  // Override `toString` for debug in REPL
  override def toString = (nuber, denom) match 
    case (0, _)  => "0"
    case (n, 1)  => s"$n"
    case (n, -1) => s"${-n}"
    case _ => s"$nuber/$denom"

  override def compare(that: Rational | Int): Int = 
    val n = that.toRational
    nuber * n.denom - n.nuber * denom

  override def equals(that: Any): Boolean =
    val n = that.asInstanceOf[Rational | Int].toRational
    (this compare n) == 0

  def == = equals
        
  def unary_+ = Rational(+nuber, +denom)
  def unary_- = Rational(-nuber, denom)

  def + (that: Rational | Int) = 
    val n = that.toRational
    Rational(nuber * n.denom + n.nuber * denom, denom * n.denom)

  def - (that: Rational | Int) =
    val n = that.toRational
    this + (-n)
  
  def * (that: Rational | Int) = 
    val n = that.toRational
    Rational(nuber * n.nuber, denom * n.denom)

  def / (that: Rational | Int) = this * that.recip
 
  def ** (n: Int): Rational = 
    val res = n.abs match 
      case 0 => Rational(1)
      case m => this ** (m - 1) * this
    if n < 0 then res.recip else res
  
  def max(that: Rational) = if this <= that then that else this
  def min(that: Rational) = if this >= that then that else this

end Rational

  
extension (x: Int)
  def + (y: Rational) = Rational(x) + y
  def * (y: Rational) = Rational(x) * y
  def - (y: Rational) = Rational(x) - y
  def / (y: Rational) = Rational(x) / y


extension(n: Rational | Int)
  def toRational = n match
    case n: Rational => n
    case n: Int      => Rational(n)

  def recip = n match
    case n: Rational => Rational(n.denom, n.nuber)
    case n: Int      => Rational(1, n)
