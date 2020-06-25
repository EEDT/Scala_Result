package com.frank.result

/**
 * @constructor 用x创建一个Err
 * @param x 该Err所包含的值
 * @tparam E x的类型
 */
case class Err[E](x:E) extends Result[Nothing,E]{
  /**
   * 返回是否为Ok
   * @return Boolean
   */
  def isOK: Boolean = false
  /**
   * 测试一个result是否包含给定值
   * 这相当与{{{someResult.x == elem}}}
   * @example {{{Err(123).contains(123) //false}}}
   * @param x 用来判断的数字
   * @return boolean
   */
  def contains(x: E): Boolean = this.x == x
  /**
   * 是否为Err，如果是Err，则返回Some(x)，否则返回None
   * @example {{{Err(123).err //Some(123)}}}
   * @return Option[E]
   */
  def err: Option[E] = Some(x)
  /**
   * 如果该result为Err，就抛出RuntimeException，消息为msg
   * @param msg 消息
   */
  def exception(msg: String): Unit = throw new RuntimeException(msg)
  /**
   * 如果该result为Err，就抛出RuntimeException，否则返回所包含的值
   * @return E
   */
  def unwrap: E = throw new NoSuchElementException("Error!")
  /**
   * 是否为Ok，如果是Ok，则返回Some(x)，否则返回None
   * @example {{{Err(123).Ok //None}}}
   * @return Option[E]
   */
  def Ok: Option[E] = None
  /**
   * 如果该result为ok，返回所包含的值，否则返回elseValue
   * @param elseValue T 否则返回的值
   * @return T
   */
  def okOrElse(elseValue: E): E = this.Ok.getOrElse(elseValue)
  /**
   * 返回迭代器
   * @return Iterator[E]
   */
  def iterator: Iterator[E] = Iterator(x)
  /**
   * 返回是否为Err
   * @return boolean
   */
  def isErr: Boolean = true
}
