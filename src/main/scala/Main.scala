import aleph.types._
import aleph.implicits._
import aleph.modes._

object Main extends App {
  println("Hello, aleph! ")

  implicit val mode = eval

  val r1 = Qint.prepare()
  val r2 = Qint.prepare(List(0, 3))

  println("* r1:")
  println(r1)

  println("* r2:")
  println(r2)

  val o1 = (6 in r1)
  println(o1.eval())

  val o2 = not (3 in r2)
  println(o2.eval())

  // val o3 = o1 or o2
  // print(o3)

  // val o4 = (6 in r1) or (12 in r1) or not (31 in r2)
  // print(o4)

  // val o5 = any(List(6, 12, 0), r1)
  // print(o5)
}
