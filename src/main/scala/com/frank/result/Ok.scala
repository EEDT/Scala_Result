package com.frank.result
/**
 * @constructor 用x创建一个Ok
 * @param x 该Ok所包含的值
 * @tparam T x的类型
 */
case class Ok[T,E](x:T) extends Result[T,E]{

  /**
   * 返回是否为Ok
   * @return Boolean
   */
  def isOK: Boolean = true
  /**
   * 是否为Err，如果是Err，则返回Some(x)，否则返回None
   * @example {{{Err(123).err //Some(123)}}}
   * @return Option[E]
   */
  def err:Option[Nothing] = None
  /**
   * 测试一个result是否包含给定值
   * 这相当与{{{someResult.x == elem}}}
   * @example {{{Err(123).contains(123) //true}}}
   * @param x 用来判断的数字
   * @return boolean
   */
  def contains(x: T): Boolean = this.x == x
  /**
   * 如果该result为Err，就抛出RuntimeException，消息为msg
   * @param msg 消息
   */
  def exception(msg: String): Unit = super.creatUnitValue()
  /**
   * 如果该result为Err，就抛出RuntimeException，否则返回所包含的值
   * @return E
   */
  def unwrap: T = x

  /**
   * 如果该result为ok，返回所包含的值，否则返回elseValue
   * @param elseValue T 否则返回的值
   * @return T
   */
  def okOrElse(elseValue:T):T = this.ok.getOrElse(elseValue)
  /**
   * 是否为Err，如果是Err，则返回Some(x)，否则返回None
   * @example {{{Err(123).err //Some(123)}}}
   * @return Option[E]
   */
  def ok: Option[T] = Some(x)
  /**
   * 返回是否为Err
   * @return boolean
   */
  def isErr: Boolean = false
  /**
   * map:为该result所包含的值执行函数f
   * @param f 函数
   * @tparam U f 的返回值
   * @return Result
   */
  def map[U](f:T => U):Result[U,E] = Ok(f(x))
  /**
   * 为该result所包含的值执行f
   * @param f 函数
   * @return Result
   */
  def flatMap(f:T => Result[T,E]): Result[T, E] = f(x)
  /**
   * 测试该result所包含的值执行f结果是否为true
   * @param f 测试函数
   * @return
   */
  def exists(f:T => Boolean): Boolean = f(x)
  /**
   * 创建seq
   * @return seq
   */
  def toSeq = Seq(x)
  /**
   * 为该result所包含的值执行f
   * @param f 函数
   */
  def foreach(f:T => Unit): Unit = f(x)
  /**
   * 返回迭代器
   * @return iterator
   */
  def iterator:Iterator[T] = Iterator(x)
}
