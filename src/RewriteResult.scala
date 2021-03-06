package tree

//import type definition of Strategy
import tree.core._
import scala.util.control.Exception._

sealed trait RewriteResult[P] {
  def getProgramOrElse(p: P): P
  def get: P

  def mapSuccess(f: P => P): RewriteResult[P]
  def flatMapSuccess(f: P => RewriteResult[P]): RewriteResult[P]

  def mapFailure(f: Strategy[P] => Strategy[P]): RewriteResult[P]
  def flatMapFailure(f: Strategy[P] => RewriteResult[P]): RewriteResult[P]
}


case class Success[P](p: P) extends RewriteResult[P] {
  override def getProgramOrElse(p: P): P = p
  override def get: P = p

  override def mapSuccess(f: P => P): RewriteResult[P] = Success(f(p))
  override def flatMapSuccess(f: P => RewriteResult[P]): RewriteResult[P] = f(p)

  override def mapFailure(f: Strategy[P] => Strategy[P]): RewriteResult[P] = this
  override def flatMapFailure(f: Strategy[P] => RewriteResult[P]): RewriteResult[P] = this
}


case class Failure[P](s: Strategy[P]) extends RewriteResult[P] {
  override def getProgramOrElse(p: P): P = p
  override def get: P = throw new Exception("Tried to get result of failed rule application")

  override def mapSuccess(f: P => P): RewriteResult[P] = this
  override def flatMapSuccess(f: P => RewriteResult[P]): RewriteResult[P] = this

  override def mapFailure(f: Strategy[P] => Strategy[P]): RewriteResult[P] = Failure(f(s))
  override def flatMapFailure(f: Strategy[P] => RewriteResult[P]): RewriteResult[P] = f(s)
}
