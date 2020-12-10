import aleph.types._

import aleph.modes._
import aleph.implicits._


object Main extends App {
  println("Hello, aleph! ")

  implicit val mode = print

  // Prepares a Qint in full super-position:
  val r1 = Qint.prepare()
  println("* r1:")
  println(r1)
  
  // Prepares a Qint in Bell-State
  val r2 = Qint.prepare(0, 3)
  println("* r2:")
  println(r2)

  // Simple oracle, 6 in 
  val o1 = (6 in r1)
  println(f"=== o1:\n${o1.eval}")

  val o2 = not (3 in r2)
  println(f"=== o2:\n${o2.eval}")

  val o3 = o1 or o2
  println(f"=== o3:\n${o3.eval}")

  val o4 = (6 in r1) and (12 in r1) or not (31 in r2)
  println(f"=== o4:\n${o4.eval}")

  val o5 = not (List(6, 12, 1) any r2)
  println(f"=== o5:\n${o5.eval}")
}
