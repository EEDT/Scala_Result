package com.frank.result
abstract class Result[T,E]{
  /**
   * 返回是否为Ok
   * @return Boolean
   */
  def isOK:Boolean = this match {
    case _: Ok[_,_] => true
    case _:Err[_,_] => false
  }
  /**
   * 测试一个result是否包含给定值
   * 这相当与{{{someResult.x == elem}}}
   * @example {{{Err(123).contains(123) //true}}}
   * @param x 用来判断的数字
   * @return boolean
   */
  def contains(x:T):Boolean = this match {
    case Ok(x) => x == x
    case Err(x) => x == x
  }
  /**
   * 返回是否为Err
   * @return boolean
   */
  def isErr:Boolean = this match {
    case _:Ok[_,_] => false
    case _:Err[_,_] => true
  }
  /**
   * 是否为Err，如果是Err，则返回Some(x)，否则返回None
   * @example {{{Err(123).err //Some(123)}}}
   * @return Option[E]
   */
  def err:Option[E] = this match {
    case Err(x) => Some(x)
    case _ => None
  }
  /**
   * 如果该result为Err，就抛出RuntimeException，消息为msg
   * @param msg 消息
   */
  def exception(msg:String):Unit = this match {
    case Err(_) => throw new RuntimeException(msg)
    case Ok(_) => creatUnitValue()
  }
  /**
   * 如果该result为Err，就抛出RuntimeException，否则返回所包含的值
   * @return E
   */
  def unwrap:T = this match {
    case Ok(x) => x
    case Err(_) => throw new RuntimeException("error")
  }
  /**
   * 如果该result为ok，返回所包含的值，否则返回elseValue
   * @param elseValue T 否则返回的值
   * @return T
   */
  def okOrElse(elseValue:T):T = this match {
    case Err(_) => elseValue
    case Ok(x) =>x
  }
  /**
   * 是否为Err，如果是Err，则返回Some(x)，否则返回None
   * @example {{{Err(123).err //Some(123)}}}
   * @return Option[E]
   */
  def ok:Option[T] = this match {
    case Ok(x) => Some(x)
    case _ => None
  }
  def iterator() = this match {
    case Ok(x) => Iterator(x)
    case Err(x) => Iterator(x)
  }
  protected def creatUnitValue() = ()
}
