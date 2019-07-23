package example

import scala.annotation.tailrec

class Trampoline extends App {

  sealed trait Trampoline[A]

  sealed trait TailRec[A] {
    def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
  }

  final case class Return[A](a: A) extends TailRec[A]
  final case class Suspend[A](resume: () => TailRec[A]) extends TailRec[A]
  final case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  case class Done[A](value: A) extends Trampoline[A]

  case class More[A](call: () => Trampoline[A]) extends Trampoline[A]

  def even[A](lst: Seq[A]): Trampoline[Boolean] = {
    lst match {
      case Nil => Done(true)
      case x :: xs => More(() => odd(xs))
    }
  }

  def odd[A](lst: Seq[A]): Trampoline[Boolean] = {
    lst match {
      case Nil => Done(false)
      case x :: xs => More(() => even(xs))
    }
  }

  case class State[S, +A](runS: S => (S, A)) {
    def map[B](f: A => B): State[S, B] =
      State[S, B](s => {
        val (s1, a) = runS(s)
        (s1, f(a))
      })

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State[S, B](s => {
        val (s1, a) = runS(s)
        f(a) runS s1
      })
  }

  println(even(1 to 1000000))

}
