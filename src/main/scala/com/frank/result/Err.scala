package com.frank.result

/**
 * @constructor 用x创建一个Err
 * @param x 该Err所包含的值
 * @tparam E x的类型
 */
case class Err[T, E](x: E) extends Result[T, E] {
  override type TypeOf = E
  override type M[B] = Result[TypeOf,B]
  /**
   * 同[[_root_.scala.util.Either]]中的map方法
   * @param f 函数
   * @tparam U 返回
   * @return result
   */
  override def map[U](f: E => U):M[U] = Err(f(x))

  /**
   * 如果该result为ok且f(x)为true，返回true
   * 否则返回false
   * @param f 函数
   * @return
   */
  def exists(f: E => Boolean): Boolean = f(x)

  /**
   * 对于该result所包含的值执行f()
   * @param f 函数
   */
  def foreach(f: E => Unit): Unit = f(x)

  /**
   * 将该result转为seq后flatmap
   */
  def flatMap[U](f: E => IterableOnce[E]):Seq[TypeOf] = {
    this.toSeq.flatMap(f)
  }

  /**
   * 创建seq
   * @return seq
   */
  def toSeq: Seq[E] = Seq(x)
}
