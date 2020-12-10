package aleph

import aleph.types._
import aleph.oracles._

object modes {
  val eval = new Operations[Boolean] {
    class O(value: Boolean) extends Oracle[Boolean] {
      def eval(): Boolean = value
    }

    override def and(
        left: Oracle[Boolean],
        right: Oracle[Boolean]
    ): Oracle[Boolean] = new O(left.eval() && right.eval())

    override def or(
        left: Oracle[Boolean],
        right: Oracle[Boolean]
    ): Oracle[Boolean] = new O(left.eval() || right.eval())

    override def not(a: Oracle[Boolean]): Oracle[Boolean] = new O(!a.eval())

    override def in(value: Int, register: Qint): Oracle[Boolean] =
      new O(register.state.contains(value))

    override def const(value: Boolean): Oracle[Boolean] = new O(value)
  }
}
