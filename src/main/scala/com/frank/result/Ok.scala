package com.frank.result
/**
 * @constructor 用x创建一个Ok
 * @param x 该Ok所包含的值
 * @tparam T x的类型
 */
case class Ok[T](x:T) extends Result[T,T]{

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
   * @example {{{Err(123).contains(123) //false}}}
   * @param x 用来判断的数字
   * @return boolean
   */
  def contains(x: T): Boolean = this.x == x
  /**
   * 返回迭代器
   * @return Iterator[T]
   */
  def iterator = Iterator(x)
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
  def okOrElse(elseValue:T) = this.Ok.getOrElse(elseValue)
  /**
   * 是否为Err，如果是Err，则返回Some(x)，否则返回None
   * @example {{{Err(123).err //Some(123)}}}
   * @return Option[E]
   */
  def Ok: Option[T] = Some(x)

  /**
   * 返回是否为Err
   * @return boolean
   */
  def isErr: Boolean = false
}
