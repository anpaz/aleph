package aleph.codegen

import aleph.types._
import aleph.oracles._

case class QubitRegister(id: Int, numChildren: Int = 1)
case class QubitTarget(qId: Int, `type`: Int = 0)

case class Operation(
    gate: String,
    targets: List[QubitTarget],
    isMeasurement: Boolean = false,
    isControlled: Boolean = false,
    controls: List[QubitTarget] = List.empty
)

case class Circuit(qubits: List[QubitRegister], operations: List[Operation]) {}

object Operations {
  val X_label = "X"

  def X(target: Qbit): List[Operation] =
    new Operation(X_label, List(QubitTarget(target.id))) :: Nil

  def CNOT(ctrls: List[Qbit], target: Qbit): List[Operation] =
    new Operation(
      gate = X_label,
      targets = List(QubitTarget(target.id)),
      controls = ctrls.map(c => new QubitTarget(qId = c.id)),
      isControlled = true
    ) :: Nil

  def OR(left: Qbit, right: Qbit, target: Qbit): List[Operation] = {
    val toggle: List[Operation] = X(left) ::: X(right)
    toggle ::: CNOT(left :: (right :: Nil), target) ::: toggle ::: X(target) ::: Nil
  }

  def AND(left: Qbit, right: Qbit, target: Qbit): List[Operation] =
    CNOT(left :: right :: Nil, target) ::: Nil

  def IN(value: Int, register: List[Qbit], target: Qbit): List[Operation] = {
    var toggle: List[Operation] = List.empty

    for (i <- 0 until register.length) {
      if ((value & (1 << i)) == 0) {
        val t = register(i)
        toggle = X(t) ::: toggle
      }
    }

    toggle ::: CNOT(register, target) ::: toggle
  }

  def CONST(value: Boolean, target: Qbit): List[Operation] =
    if (value) X(target) else Nil

}

trait Program {
  val registers: Set[Qint]
  val ancillas: Set[Qbit]
  val target: Qbit
  val ops: List[Operation]
}

case class JsonProgram(r: Set[Qint], a: Set[Qbit], t: Qbit, o: List[Operation]) extends Program {
  val registers: Set[Qint] = r
  val ancillas: Set[Qbit]  = a
  val target: Qbit         = t
  val ops: List[Operation] = o

  val circuit = {
    val qubits = (registers.flatMap(r => r.qubits) ++ ancillas + target).toList.sortWith((a, b) => a.id < b.id).map(q => QubitRegister(id = q.id))
    Circuit(qubits, ops)
  }

  import net.liftweb.json.DefaultFormats
  import net.liftweb.json.Serialization.{write, writePretty}

  implicit val formats = DefaultFormats

  override def toString(): String = {
    write(circuit)
  }
}

case class QshaprProgram(r: Set[Qint], a: Set[Qbit], t: Qbit, o: List[Operation]) extends Program {
  val registers: Set[Qint] = r
  val ancillas: Set[Qbit]  = a
  val target: Qbit         = t
  val ops: List[Operation] = o

  val operation = {
    def id_label(id: Int): String = f"q_${id}"

    def qbit_var(q: Qbit): String = id_label(q.id)

    def deconstruct_qint(a: (Qint, Int)): String = {
      val qs = a._1.qubits.map(q => qbit_var(q)).mkString(",")
      f"        let ($qs) = r_${a._2};"
    }

    def deconstruct_target(): String =
      f"        let ${qbit_var(t)} = target;"

    def generate_instruction(op: Operation): String = {
      val target = id_label(op.targets.head.qId)
      if (op.isControlled) {
        var ctrls = op.controls.map(c => id_label(c.qId)).mkString(",")
        f"            Controlled ${op.gate}([$ctrls], $target);"

      } else {
        f"            ${op.gate}($target);"
      }
    }

    val start_using: String =
      if (a.isEmpty) {
        ""
      } else {
        var ancillas_vars  = a.map(qbit_var).mkString(",")
        var ancilla_qubits = a.toList.map(q => "Qubit()").mkString(",")
        f"        using(($ancillas_vars) = ($ancilla_qubits)) {"
      }

    val end_using: String =
      if (a.isEmpty) "" else "        }"

    var indexed_regs = r.zipWithIndex

    var parameters   = (indexed_regs.map(t => f"r_${t._2}: Qubit[]") ++ Set("target: Qubit")).mkString(",")
    var deconstructs = (indexed_regs.map(deconstruct_qint) ++ Set(deconstruct_target)).mkString("\n")
    var instructions = ops.map(generate_instruction).mkString("\n")

    f""""
    operation Oracle(${parameters}) : Unit
    is Adj + Ctl {
$deconstructs
$start_using
$instructions
$end_using
    }
    """"
  }

  override def toString(): String = operation
}

class ProgramOps(pFactory: (Set[Qint], Set[Qbit], Qbit, List[Operation]) => Program)(implicit qfactory: QbitFactory) extends Operations[Program] {
  import Operations._

  class O(p: Program) extends Oracle[Program] {
    val eval: Program = p
  }

  def binary(
      left: Oracle[Program],
      right: Oracle[Program],
      instructions: (Qbit, Qbit, Qbit) => List[Operation]
  ): Oracle[Program] = {
    val l        = left.eval
    val r        = right.eval
    val register = l.registers ++ r.registers
    val ancillas = l.ancillas ++ r.ancillas + l.target + r.target
    val target   = qfactory.allocate()
    var program  = pFactory(register, ancillas, target, l.ops ::: r.ops ::: instructions(l.target, r.target, target))

    new O(program)
  }

  override def and(
      left: Oracle[Program],
      right: Oracle[Program]
  ): Oracle[Program] = binary(left, right, AND)

  override def or(
      left: Oracle[Program],
      right: Oracle[Program]
  ): Oracle[Program] = binary(left, right, OR)

  override def not(a: Oracle[Program]): Oracle[Program] = {
    val l       = a.eval
    var program = pFactory(l.registers, l.ancillas, l.target, l.ops ::: X(l.target))

    new O(program)
  }

  override def in(value: Int, register: Qint): Oracle[Program] = {
    val target  = qfactory.allocate()
    val program = pFactory(Set(register), Set.empty, target, IN(value, register.qubits, target))

    new O(program)
  }

  override def const(value: Boolean): Oracle[Program] = {
    val target  = qfactory.allocate()
    val program = pFactory(Set.empty, Set.empty, target, CONST(value, target))

    new O(program)
  }
}
