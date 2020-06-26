package com.frank.result
abstract class Result[T,E]{
  def isOK:Boolean = this match {
    case _: Ok[_,_] => true
    case _:Err[_,_] => false
  }
  def contains(x:T):Boolean = this match {
    case Ok(x) => x == x
    case Err(x) => x == x
  }
  def isErr:Boolean = this match {
    case _: Ok[_, _] => false
    case _: Err[_, _] => true
  }
  def err:Option[E] = this match {
    case Err(x) => Some(x)
    case _ => None
  }
  def exception(msg:String):Unit = this match {
    case Err(_) => throw new RuntimeException(msg)
    case Ok(_) => creatUnitValue()
  }
  def unwrap:T = this match {
    case Ok(x) => x
    case Err(_) => throw new RuntimeException("error")
  }
  def okOrElse(elseValue:T):T = this match {
    case Err(_) => elseValue
    case Ok(x) =>x
  }
  def ok:Option[T] = this match {
    case Ok(x) => Some(x)
    case _ => None
  }
  def iterator() = this match {
    case Ok(x) => Iterator(x)
    case Err(x) => Iterator(x)
  }
  def map[U](f:T => U):Result[U,E] = this match {
    case Ok(x) => Ok(f(x))
    case _ => this.asInstanceOf[Result[U,E]]
  }
  def filter(f:T => Boolean):Result[T,E] = this match {
    case Ok(x) if f(x) => Ok(x)
    case _ => this
  }
  def exists(f:T => Boolean) = this match {
    case Err(_) => false
    case Ok(x) => f(x)
  }
  def flatMap(f:T => Result[T,E]) = this match {
    case Err(_) => this.asInstanceOf[Result[T,E]]
    case Ok(x) => f(x)
  }
  def foreach(f:T => Unit) = this match {
    case Ok(x) => f(x)
    case _ => creatUnitValue()
  }
  protected def creatUnitValue() = ()
}
