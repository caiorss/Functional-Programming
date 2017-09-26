
package complex

case class Cpl(re: Double, img: Double){

  override def toString() =
    s"${re} + ${img}j"

  def unary_-() = 
    Cpl(-this.re, -this.img)

  def +(that: Double) = 
    Cpl(this.re + that, this.img)


  def +(that: Cpl) = 
    Cpl(this.re + that.img, this.img + that.img)


  def -(that: Double) = 
    Cpl(this.re - that, this.img)
  

  def -(that: Cpl) = 
    Cpl(this.re - that.re, this.img - that.img)
  

  def *(that: Double) =
    Cpl(that * this.re, that * this.img)

  def *(that: Cpl) = {
    val x = this.re * that.re - this.img * that.img
    val y = this.re * that.img + this.img * that.re 
    Cpl(x, y)
    }


  def /(that: Double) =
    Cpl(this.re / that, this.img / that)

  def /(that: Cpl) = {
    val r = that.re * that.re + that.img * that.img
    val x = this.re * that.re + this.img * that.img
    val y = - this.re * that.img + this.img * that.re
    Cpl(x / r, y / r)
  }
    
  // Conjugate 
  def conj =
    Cpl(this.re, -this.img)

  def norm = Math.sqrt(re * re + img * img)

  // Angle in radians 
  def angle = Math.atan2(img, re)

  // Angle in degrees 
  def angled = Math.atan2(img, re) * 180.0 / Math.PI 

}


object Complex{

  val j = Cpl(0, 1)

  implicit class DoubleToCpl(k: Double) {

    //def j = Cpl(0, k)

    def polr = Cpl(Math.cos(k), Math.sin(k))

    def pold = {
      val a = k / 180.0 * Math.PI
      Cpl(Math.cos(a), Math.sin(a))
    }

    // k + that
    def +(that: Cpl) = Cpl(that.re + k, that.img)

    // k * that 
    def *(that: Cpl) = Cpl(k * that.re, k * that.img)

    def -(that: Cpl) = Cpl(k - that.re, -that.img)

    // k / that    
    def /(that: Cpl) = {
      val c = that.re * that.re + that.img * that.img
      Cpl( k * that.re / c, - k * that.img / c)
    }
  }
}
