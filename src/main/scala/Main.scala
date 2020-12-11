import aleph.types._

import aleph.modes._
import aleph.implicits._

object Main extends App {
  println("Hello, aleph! ")

  implicit val mode = print

  // Prepares a Qint at a specific value
  val r0 = Qint.prepare(6)
  println("* r0:")
  println(r0)

  // Prepares a Qint in full super-position:
  val r1 = Qint.prepare()
  println("* r1:")
  println(r1)

  // Prepares a Qint in Bell-State
  val r2 = Qint.prepare(0, 3)
  println("* r2:")
  println(r2)

  // Constant oracle
  var t = true.oracle
  println("=== t:")
  println(t.eval)

  // Simple oracle, 3 in r2
  val o1 = (3 in r2)
  println(f"=== o1:\n${o1.eval}")

  // Apply negation:
  val o2 = not(6 in r1)
  println(f"=== o2:\n${o2.eval}")

  // Combine oracles using or
  val o3 = o1 or o2
  println(f"=== o3:\n${o3.eval}")

  // More complex expression:
  val o4 = (6 in r1) and (12 in r1) or not(1 in r2)
  println(f"=== o4:\n${o4.eval}")

  // Leverage `any` and `all`. They can be combined as any other oracle
  val o5 = (List(6, 12, 1) any r1) and (List(0, 1) all r2)
  println(f"=== o5:\n${o5.eval}")
}
