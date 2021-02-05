package com.frank.result

import com.frank.result.Result.result2Either

import scala.language.implicitConversions
import scala.util._


/**
 * 一个为[[scala.util.Either]]提供了更多特性的库
 *
 * @tparam T 正常
 * @tparam E 错误
 */
trait Result[+T, +E] extends Any {

  type TypeOf

  /**
   * 返回是否为ok
   *
   * @return boolean
   */
  def isOK: Boolean = this match {
    case Ok(_) => true
    case Err(_) => false
  }

  /**
   * 测试一个result是否包含给定值且该result为OK
   *
   * @param i 测试值
   * @return boolean
   */
  def contains[M >: T](i: M): Boolean = this match {
    case Ok(x) => x == i
    case _ => false
  }

  /**
   * 测试一个result是否包含给定值且该result为Err
   *
   * @param x 测试值
   * @return boolean
   */
  def containsErr[M >: T](x: M): Boolean = this match {
    case Err(x) => x == x
    case Ok(_) => false
  }

  /**
   * 是否为err
   *
   * @return boolean
   */
  def isErr: Boolean = this match {
    case Err(_) => true
    case _ => false
  }

  /**
   * 如果是err，返回some，否则返回none
   *
   * @return option
   */
  def err: Option[E] = this match {
    case Err(x) => Some(x)
    case _ => None
  }

  /**
   * 如果不是ok，抛出[[java.lang.RuntimeException]]
   *
   * @param msg 异常信息
   */
  def exception(msg: String): Unit = this match {
    case Err(_) => throw new RuntimeException(msg)
    case Ok(_) => creatUnitValue()
  }

  /**
   * 如果不是err，抛出[[java.lang.RuntimeException]]
   *
   * @param msg 异常信息
   */
  def exceptionErr(msg: String): Unit = this match {
    case Err(_) => creatUnitValue()
    case _ => throw new RuntimeException(msg)
  }

  /**
   * 如果不是ok，抛出[[java.lang.RuntimeException]]，否则返回ok中包含的值
   *
   * @return
   */
  def unwrap: T = this match {
    case Ok(x) => x
    case Err(_) => throw new RuntimeException("not ok")
  }

  /**
   * 如果不是err，抛出[[java.lang.RuntimeException]]，否则返回err中包含的值
   *
   * @return
   */
  def unwrapErr: E = this match {
    case Err(x) => x
    case Ok(_) => throw new RuntimeException("not err")
  }

  /**
   * 如果不是ok，返回default，否则返回ok中包含的值
   */
  def unwrapOrDefault[M >: T](default: M): M = this match {
    case Err(_) => default
    case Ok(x) => x
  }

  /**
   * 如果是Ok返回ok所包含的值，否则返回e(err所包含的值)
   *
   * @param e 函数
   * @return T
   */
  def unwrapOrElse[M1 >: T, M2 >: E](e: M2 => M1): M1 =
    this match {
      case Err(x) => e(x)
      case Ok(x) => x
    }

  /**
   * 如果是OK，返回所包含的值，否则返回elseValue
   *
   * @param elseValue 否则返回的值
   * @return t
   */
  def okOrElse[M >: T](elseValue: M): M = this match {
    case Err(_) => elseValue
    case Ok(x) => x
  }

  /**
   * 如果是ok，返回some，否则返回none
   *
   * @return option
   */
  def ok: Option[T] = this match {
    case Ok(x) => Some(x)
    case _ => None
  }

  /**
   * 返回迭代器
   *
   * @return iterator
   */
  def iterator(): Iterator[TypeOf]

  /**
   * 同[[scala.util.Either]]中的map方法
   *
   * @param f 函数
   * @tparam U 返回
   * @return result
   */
  def map[U](f: T => U): Result[U, E]

  /**
   *
   * @param f 函数
   * @tparam U 返回
   * @return [[com.frank.result.Result]]
   */
  def mapErr[U](f: E => U): Result[T, U] = this match {
    case Err(x) => Err(f(x))
    case Ok(x) => Ok(x).asInstanceOf[Result[T, U]]
  }

  /**
   * 如果该result为ok且f(x)为true，返回true
   * 否则返回false
   *
   * @param f 函数
   * @return
   */
  def exists(f: TypeOf => Boolean): Boolean

  /**
   * 将该result转为seq后flatmap
   */
  def flatMap[M >: T, U >: E](f: T => Result[M, U]): Result[M,U] =
    Result.fromEither(this.toEither.flatMap(x => result2Either(f(x))))

    

  /**
   * 如果是ok，执行seq(x).flatMap，否则执行seq(x).flatMap(e)
   *
   * @param f 函数
   * @param e 函数
   * @return Seq[T]
   */
  def flatMapOrElse[M >: T, ME >: E](
                                      f: T => Result[M, ME],
                                      e: E => Result[M, ME]
                                    ): Result[M, ME] =
    this match {
      case Err(x) => e(x)
      case Ok(x) => f(x)
    }

  /**
   * 将该result转为seq后flatten
   *
   * @param f 函数
   * @return seq
   */
  def flatten[M >: T](implicit f: T => IterableOnce[M]): Seq[M] = this match {
    case Err(_) => Seq()
    case Ok(x) => Seq(x).flatten(f)
  }

  /**
   * 如果该result为Ok则执行函数f，否则执行函数default
   *
   * @param f       为Ok则执行的函数
   * @param default 为Err时执行的函数
   * @tparam U 结果
   * @return [[com.frank.result.Result]]
   */
  def mapOrElse[U](f: T => U, default: E => U): Result[U, U] =
    this match {
      case Err(x) => Err(default(x))
      case Ok(x) => Ok(f(x))
    }

  /**
   * 如果该result为Ok则返回this，否则返回default
   *
   * @param result 否则的值
   * @return [[com.frank.result.Result]]
   */
  def or[M1 >: T, M2 >: E](result: Result[M1, M2]): Result[M1, M2] =
    this match {
      case Err(_) => result
      case _ => this
    }

  /**
   * 如果该result为Ok则返回this，否则返回对该result所包含的值执行func
   *
   * @param func 函数
   * @return [[com.frank.result.Result]]
   */
  def orElse[M1 >: T, M2 >: E](func: M2 => Result[M1, M2]): Result[M1, M2] =
    this match {
      case Err(x) => func(x)
      case Ok(_) => this
    }

  /**
   * 对于该result所包含的值执行f()
   *
   * @param f 函数
   */
  def foreach(f: T => Unit): Unit

  def foreachOrElse(f: T => Unit, default: E => Unit): Unit =
    this match {
      case Err(x) => default(x)
      case Ok(x) => f(x)
    }

  /**
   * 创建seq
   *
   * @return seq
   */
  def toSeq: Seq[T]

  protected def creatUnitValue(): Unit = ()

  /**
   * 创建一个Try
   *
   * @return try
   */
  def toTry: Try[T] =
    this match {
      case Err(x) => Failure(x.asInstanceOf[Throwable])
      case Ok(x) => Success(x)
    }

  /**
   * 创建一个Either
   *
   * @return either
   */
  def toEither: Either[E, T] =
    this match {
      case Err(x) => Left(x)
      case Ok(x) => Right(x)
    }
}

object Result {
  def fromEither[A, B](x: scala.util.Either[B, A]): Result[A, B] =
    x match {
      case Left(value) => Err(value)
      case Right(value) => Ok(value)
    }

  /**
   * 将either转为result
   */
  implicit def either2Result[A, B](x: scala.util.Either[B, A]): Result[A, B] =
    Result.fromEither(x)

  implicit def result2Either[A, B](x: Result[A, B]): Either[B, A] =
    x match {
      case Err(x) => Left(x)
      case Ok(x) => Right(x)
    }
}
