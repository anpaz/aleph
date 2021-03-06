package aleph

import aleph.types._
import aleph.oracles._

// The type class interface, so the operations can be used implicitly:
object implicits {
  implicit class InOracle[T](a: Int) {
    def in(register: Qint)(implicit o: Operations[T]): Oracle[T] = o.in(a, register)
  }

  implicit class ConstOracle[T](a: Boolean)(implicit o: Operations[T]) {
    def oracle()(implicit o: Operations[T]): Oracle[T] = o.const(a)
  }

  implicit class ListOracle[T](a: List[Int]) {
    def any(register: Qint)(implicit o: Operations[T]): Oracle[T] =
      a match {
        case head :: next => o.or(o.in(head, register), (next any register))
        case Nil          => o.const(false)
      }

    def all(register: Qint)(implicit o: Operations[T]): Oracle[T] =
      a match {
        case head :: next => o.and(o.in(head, register), (next all register))
        case Nil          => o.const(true)
      }
  }

  implicit class OracleExpressions[T](a: Oracle[T])(implicit o: Operations[T]) {
    def and(b: Oracle[T]): Oracle[T] = o.and(a, b)
    def or(b: Oracle[T]): Oracle[T]  = o.or(a, b)
    def not(): Oracle[T]             = o.not(a)
  }

  def not[T](a: Oracle[T])(implicit o: Operations[T]): Oracle[T] = o.not(a)

  implicit val qubits_factory = QbitFactory
}
