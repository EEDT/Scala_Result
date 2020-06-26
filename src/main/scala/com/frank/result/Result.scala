package com.frank.result
/**
 * 一个为[[scala.util.Either]]提供了更多特性的库
 * @tparam T 正常
 * @tparam E 错误
 */
abstract class Result[T,E]{
  /**
   * 返回是否为ok
   * @return boolean
   */
  def isOK:Boolean = this match {
    case _: Ok[_,_] => true
    case _:Err[_,_] => false
  }
  /**
   * 测试一个result是否包含给定值
   * @param x 测试值
   * @return boolean
   */
  def contains(x:T):Boolean = this match {
    case Ok(x) => x == x
    case Err(x) => x == x
  }

  /**
   * 是否为err
   * @return boolean
   */
  def isErr:Boolean = this match {
    case _: Ok[_, _] => false
    case _: Err[_, _] => true
  }

  /**
   * 如果是err，返回some，否则返回none
   * @return option
   */
  def err:Option[E] = this match {
    case Err(x) => Some(x)
    case _ => None
  }

  /**
   * 如果不是ok，抛出[[java.lang.RuntimeException]]
   * @param msg 异常信息
   */
  def exception(msg:String):Unit = this match {
    case Err(_) => throw new RuntimeException(msg)
    case Ok(_) => creatUnitValue()
  }

  /**
   * 如果不是ok，抛出[[java.lang.RuntimeException]]，否则返回ok中包含的值
   * @return
   */
  def unwrap:T = this match {
    case Ok(x) => x
    case Err(_) => throw new RuntimeException("error")
  }

  /**
   * 如果是OK，返回所包含的值，否则返回elseValue
   * @param elseValue 否则返回的值
   * @return t
   */
  def okOrElse(elseValue:T):T = this match {
    case Err(_) => elseValue
    case Ok(x) =>x
  }

  /**
   * 如果是ok，返回some，否则返回none
   * @return option
   */
  def ok:Option[T] = this match {
    case Ok(x) => Some(x)
    case _ => None
  }

  /**
   * 返回迭代器
   * @return iterator
   */
  def iterator() = this match {
    case Ok(x) => Iterator(x)
    case Err(x) => Iterator(x)
  }

  /**
   * 同[[scala.util.Either]]中的map方法
   * @param f 函数
   * @tparam U 返回
   * @return result
   */
  def map[U](f:T => U):Result[U,E] = this match {
    case Ok(x) => Ok(f(x))
    case _ => this.asInstanceOf[Result[U,E]]
  }

  /**
   * 如果该result是ok且f(this)为true，返回该result
   * 否则返回else
   * @param f 函数
   * @param Else 否则
   * @return result
   */
  def filter(f:T => Boolean,Else:Result[T,E]):Result[T,E] = this match {
    case Ok(x) if f(x) => Ok(x)
    case _ => Else
  }

  /**
   * 如果该result为ok且f(x)为true，返回true
   * 否则返回false
   * @param f 函数
   * @return
   */
  def exists(f:T => Boolean) = this match {
    case Err(_) => false
    case Ok(x) => f(x)
  }

  /**
   * 同[[scala.util.Either]]的flatmap
   */
  def flatMap(f:T => Result[T,E]) = this match {
    case Err(_) => this.asInstanceOf[Result[T,E]]
    case Ok(x) => f(x)
  }

  /**
   * 对于该result所包含的值执行f()
   * @param f 函数
   */
  def foreach(f:T => Unit) = this match {
    case Ok(x) => f(x)
    case _ => creatUnitValue()
  }
  protected def creatUnitValue() = ()
}
