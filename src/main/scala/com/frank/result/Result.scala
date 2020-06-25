package com.frank.result
abstract class Result[T,E](x:T) extends Iterable[T]{
  def isOK:Boolean
  def err:Option[E]
  def exception(msg:String):Unit
  def unwrap:T
  def okOrElse(elseValue:T):T
  def Ok:Option[T]
  protected def creatUnitValue() = ()
}
