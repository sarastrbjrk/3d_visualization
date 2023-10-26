package engine

class Vector2D(var x: Double, var y: Double) {

  def length = Math.sqrt(x*x + y*y)

  def +(other:Vector2D): Vector2D = new Vector2D(x + other.x, y + other.y)

  def -(other:Vector2D): Vector2D = new Vector2D(x - other.x, y - other.y)

  def *(d: Double): Vector2D = new Vector2D(x * d, y * d)

  def /(d: Double): Vector2D = this * (1/d)

}
