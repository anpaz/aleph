package aleph.types

trait QbitFactory {
  def allocate(): Qbit
  def allocate(n: Int): List[Qbit]
}

object QbitFactory extends QbitFactory {
  var last: Int = -1

  def allocate(): Qbit = {
    last += 1
    QbitImpl(last)
  }

  def allocate(n: Int): List[Qbit] = {
    n match {
      case 0 => Nil
      case 1 => allocate() :: Nil
      case x => allocate() :: allocate(x - 1)
    }
  }
}

trait Qbit {
  val id: Int
}
case class QbitImpl(i: Int) extends Qbit {
  val id: Int                     = i
  override def toString(): String = f"$id"
}

trait Qint {
  val qubits: List[Qbit]
  val state: List[Int]
}

class Qint_n(size: Int, values: List[Int])(implicit factory: QbitFactory) extends Qint {

  val qubits = factory.allocate(size)

  val state = values

  override def toString(): String = f"| qubits: ${qubits} +++ state:  ${state} >"
}

object Qint {
  def prepare(values: Int*)(implicit factory: QbitFactory): Qint = {
    if (values.isEmpty) {
      new Qint_n(4, List.range(0, 16))
    } else {
      var i: Int = 1
      val max    = values.max

      while ((1 << i) < max) {
        i += 1
      }

      new Qint_n(i, values.toList)
    }
  }
}
