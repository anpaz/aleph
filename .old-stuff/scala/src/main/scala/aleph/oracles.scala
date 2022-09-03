package aleph.oracles

import aleph.types.Qint

// An Oracle class. An oracle register a Register of type R,
// an returns its action on a target of type T
trait Oracle[T] {
  val eval: T
}

// Oracles can be combined using boolean operations: and, or, not.
trait Operations[T] {
  def in(value: Int, register: Qint): Oracle[T]
  def const(value: Boolean): Oracle[T]

  def and(left: Oracle[T], right: Oracle[T]): Oracle[T]
  def or(left: Oracle[T], right: Oracle[T]): Oracle[T]
  def not(a: Oracle[T]): Oracle[T]
}
