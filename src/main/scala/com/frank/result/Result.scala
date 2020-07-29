package com.frank.result

import scala.util._


/**
 * 一个为[[scala.util.Either]]提供了更多特性的库
 * @tparam T 正常
 * @tparam E 错误
 */
trait Result[T,E] extends Any{

  type TypeOf
  /**
   * 返回是否为ok
   * @return boolean
   */
  def isOK:Boolean = this match {
    case _: Ok[_,_] => true
    case _:Err[_,_] => false
  }
  /**
   * 测试一个result是否包含给定值且该result为OK
   * @param i 测试值
   * @return boolean
   */
  def contains(i:T):Boolean = this match {
    case Ok(x) => x == i
    case _ => false
  }
  /**
   * 测试一个result是否包含给定值且该result为Err
   * @param x 测试值
   * @return boolean
   */
  def containsErr(x:E):Boolean = this match {
    case Err(x) => x == x
    case Ok(_) => false
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
   * 如果不是ok，抛出[[_root_.java.lang.RuntimeException]]
   * @param msg 异常信息
   */
  def exception(msg:String):Unit = this match {
    case Err(_) => throw new RuntimeException(msg)
    case Ok(_) => creatUnitValue()
  }

  /**
   * 如果不是err，抛出[[_root_.java.lang.RuntimeException]]
   * @param msg 异常信息
   */
  def exceptionErr(msg:String):Unit = this match {
    case Err(_) => creatUnitValue()
    case _ => throw new RuntimeException(msg)
  }
  /**
   * 如果不是ok，抛出[[_root_.java.lang.RuntimeException]]，否则返回ok中包含的值
   * @return
   */
  def unwrap:T = this match {
    case Ok(x) => x
    case Err(_) => throw new RuntimeException("not ok")
  }
  /**
   * 如果不是err，抛出[[java.lang.RuntimeException]]，否则返回err中包含的值
   * @return
   */
  def unwrapErr:E = this match {
    case Err(x) => x
    case Ok(_) => throw new RuntimeException("not err")
  }
  /**
   * 如果不是ok，返回default，否则返回ok中包含的值
   */
  def unwrapOrDefault(default:T):T = try this.unwrap catch {
    case _:RuntimeException => default
  }

  /**
   * 如果是Ok返回ok所包含的值，否则返回e(err所包含的值)
   * @param e 函数
   * @return T
   */
  def unwrapOrElse(e:E => T):T =
    this match {
      case Err(x) => e(x)
      case Ok(x) => x
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
  def iterator():Iterator[TypeOf]
  /**
   * 同[[scala.util.Either]]中的map方法
   * @param f 函数
   * @tparam U 返回
   * @return result
   */
  def map[U](f:T => U):Result[U,E]
  /**
   * 如果该result为ok且f(x)为true，返回true
   * 否则返回false
   * @param f 函数
   * @return
   */
  def exists(f:TypeOf => Boolean):Boolean

  /**
   * 将该result转为seq后flatmap
   */
  def flatMap[U](f:TypeOf => IterableOnce[TypeOf]):Seq[TypeOf]

  /**
   * 将该result转为seq后flatten
   * @param f 函数
   * @return seq
   */
  def flatten(implicit f:TypeOf => IterableOnce[TypeOf]):Seq[TypeOf]

  /**
   * 如果该result为Ok则执行函数f，否则执行函数default
   * @param f 为Ok则执行的函数
   * @param default 为Err时执行的函数
   * @tparam U 结果
   * @return [[com.frank.result.Result]]
   */
  def mapOrElse[U](f:T => U, default:E => U):Result[U,U] =
    this match {
      case Err(x) => Err(default(x))
      case Ok(x) => Ok(f(x))
    }

  /**
   * 如果该result为Ok则返回this，否则返回default
   * @param result 否则的值
   * @return [[com.frank.result.Result]]
   */
  def or(result: Result[T,E]):Result[T,E] =
    this match {
      case Err(_) => result
      case _ => this
    }

  /**
   *  如果该result为Ok则返回this，否则返回对该result所包含的值执行func
   * @param func 函数
   * @return [[com.frank.result.Result]]
   */
  def orElse(func:E => Result[T,E]):Result[T,E] =
    this match {
      case Err(x) => func(x)
      case Ok(_) => this
    }
  /**
   * 对于该result所包含的值执行f()
   * @param f 函数
   */
  def foreach(f:TypeOf => Unit):Unit

  /**
   * 创建seq
   * @return seq
   */
  def toSeq:Seq[TypeOf]
  protected def creatUnitValue() = ()

  /**
   * 创建一个Try
   * @return try
   */
  def toTry:Try[T] = this match {
    case Err(x) => Failure(x.asInstanceOf[Throwable])
    case Ok(x) => Success(x)
  }

  /**
   * 创建一个Either
   * @return either
   */
  def toEither:Either[E,T] = this match {
    case Err(x) => Left(x)
    case Ok(x) => Right(x)
  }
}
object Result{
  def fromEither[A,B](x:scala.util.Either[B,A]):Result[A,B] = x match {
    case Left(value) => Err(value)
    case Right(value) => Ok(value)
  }
}
