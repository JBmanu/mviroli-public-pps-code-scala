package u07.traits

case class Point(x: Int, y: Int)

trait Rectangular:
  // abstract methods
  def topLeft: Point
  def bottomRight: Point
  // template methods
  def left: Int = topLeft.x
  def right: Int = bottomRight.x
  def top: Int = topLeft.y
  def bottom: Int = bottomRight.y

case class Rectangle(
  override val topLeft: Point,
  override val bottomRight:Point) extends Rectangular

object TryRichInterface extends App:
  val r = Rectangle(Point(0,0),Point(10,20))
  println(s"${r.left},${r.right}")
  println(s"${r.top},${r.bottom}")
