package wop.game

import TicTacToe._

import scala.annotation.tailrec

case class TicTacToe[T](size: Int, matrix: Vector[T])(implicit item: Item[T]) {

  private val boundary = size - 1

  def set(point: Point, value: T): TicTacToe[T] = {
    val (x, y) = point
    copy(matrix = matrix.updated(x + y * size, value))
  }

  def apply(point: Point): T = {
    val (x, y) = point
    matrix(x + y * size)
  }

  lazy val status: Status[T] = {

    def scan2d(f: Point => T): Option[T] = {
      @tailrec def loop(acc: Boolean, prev: T, i: Int, j: Int): Option[T] = {
        if (j == size) {
          if (acc) Some(prev)
          else if (i < boundary) loop(acc = true, f(i + 1, 0), i + 1, 1)
          else None
        } else {
          val curr = f(i, j)
          loop(acc && item.compare(curr, prev), curr, i, j + 1)
        }
      }
      loop(acc = true, f(0, 0), 0, 1)
    }

    def diagonal: Option[T] = {
      @tailrec def loop(leftAcc: Boolean, rightAcc: Boolean, leftPrev: T, rightPrev: T, i: Int): Option[T] = {
        i == size match {
          case false =>
            val leftCurr = apply(i, i)
            val j = size - 1 - i
            val rightCurr = apply(j, i)
            loop(leftAcc = leftAcc && item.compare(leftCurr, leftPrev),
                 rightAcc = rightAcc && item.compare(rightCurr, rightPrev),
                 leftPrev = leftCurr,
                 rightPrev = rightCurr,
                 i = i + 1)
          case _ if leftAcc => Some(leftPrev)
          case _ if rightAcc => Some(rightPrev)
          case _ => None
        }
      }
      loop(leftAcc = true, rightAcc = true, apply(0, 0), apply(boundary, 0), 1)
    }

    def horizontal = scan2d { case (i, j) => apply((j, i)) }

    def vertical = scan2d(apply)

    diagonal orElse horizontal orElse vertical match {
      case Some(x) => Status.Finished(x)
      case None if matrix.forall(item.nonEmpty) => Status.Draw
      case _ => Status.NotFinished
    }
  }
}

object TicTacToe {

  trait Item[T] {
    def compare(a: T, b: T): Boolean
    def nonEmpty(v: T): Boolean
  }

  sealed trait Status[+W] {
    def finished: Boolean
  }

  object Status {
    case class Finished[+W](item: W) extends Status[W] { val finished = true }
    case object NotFinished extends Status[Nothing] { val finished = false }
    case object Draw extends Status[Nothing] { val finished = true }
  }

  def apply[T](size: Int, v: T)(implicit item: Item[T]): TicTacToe[T] = {
    TicTacToe(size, Vector.fill[T](size * size)(v))
  }
}
