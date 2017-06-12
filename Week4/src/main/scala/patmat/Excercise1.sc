abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true

  def predecessor: Nat = Zero

  def successor: Nat = new Succ(Zero)

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = Zero

  override def toString: String = "0"
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false

  def predecessor: Nat = n

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat = if (that.isZero) this else successor + that.predecessor

  def -(that: Nat): Nat = if (that.isZero) this else predecessor - that.predecessor

  override def toString: String = predecessor.toString() + "+"
}

val five = Zero.successor.successor.successor.successor.successor
val two = new Succ(new Succ(Zero))

val three = five - two

val four = three + Zero.successor