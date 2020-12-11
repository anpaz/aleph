package aleph.types

trait QbitFactory {
  def allocate(): Qbit
  def allocate(n: Int): List[Qbit]
}

object QbitFactory extends QbitFactory {
  var next: Int = 0

  def allocate(): Qbit = {
    next += 1
    new Qbit(next)
  }

  def allocate(n: Int): List[Qbit] = {
    n match {
      case 0 => Nil
      case 1 => allocate() :: Nil
      case x => allocate() :: allocate(x - 1)
    }
  }
}


class Qbit(i: Int) {
  override def toString(): String = f"$id"
  val id: Int = i
}

trait Qint {
  val qubits: List[Qbit]
  val state: List[Int]
}

class Qint_n(size: Int, values: List[Int])(implicit factory: QbitFactory)
    extends Qint {

  val qubits = factory.allocate(size)

  val state = values

  override def toString(): String = f"| qubits: ${qubits} +++ state:  ${state} >"
}

object Qint {

  def prepare(values: Int*)(implicit factory: QbitFactory) : Qint = {
    if (values.isEmpty) {
      new Qint_n(4, List.range(0, 16))
    } else {
      var i: Int = 1
      val max = values.max

      while((1 << i) < max) {
        i += 1
      }
      
      new Qint_n(i, values.toList)
    }
  }
}

// class Qint(value: Qint) (implicit o: Operations[T]) {
//   def =+(rhs: Qint): Qint = o.add(this, rhs)
//   def =-(rhs: Qint): Qint = o.subtract(value, rhs)
// }

// // -----------------------------------------------------------------
// // This type class with all the operations we will need to do to
// // implement the Field/Trig for Complex:
// // -----------------------------------------------------------------
// sealed trait Operations[T] {
//   def const(c: Int): Qint
//   def add(a: Qint, b: Qint): Qint
//   def subtract(a: Qint, b: Qint): Qint
//   def negate(a: Qint): Qint
// }
