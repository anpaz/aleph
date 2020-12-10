package aleph

import aleph.types._
import aleph.oracles._

object modes {

  val eval = new Operations[Boolean] {
    class O(value: Boolean) extends Oracle[Boolean] {
      val eval: Boolean = value
    }

    override def and(
        left: Oracle[Boolean],
        right: Oracle[Boolean]
    ): Oracle[Boolean] = new O(left.eval && right.eval)

    override def or(
        left: Oracle[Boolean],
        right: Oracle[Boolean]
    ): Oracle[Boolean] = new O(left.eval || right.eval)

    override def not(a: Oracle[Boolean]): Oracle[Boolean] = new O(!a.eval)

    override def in(value: Int, register: Qint): Oracle[Boolean] =
      new O(register.state.contains(value))

    override def const(value: Boolean): Oracle[Boolean] = new O(value)
  }

  val print = new Operations[String] {
    class O(value: String) extends Oracle[String] {
      val eval: String = value
    }

    override def and(
        left: Oracle[String],
        right: Oracle[String]
    ): Oracle[String] = new O(f"(${left.eval} and ${right.eval})")

    override def or(
        left: Oracle[String],
        right: Oracle[String]
    ): Oracle[String] = new O(f"(${left.eval} or ${right.eval})")

    override def not(a: Oracle[String]): Oracle[String] = new O(f"not (${a.eval})")

    override def in(value: Int, register: Qint): Oracle[String] =
      new O(f"$value in ${register.qubits}")

    override def const(value: Boolean): Oracle[String] = new O(value.toString())
  }
}
