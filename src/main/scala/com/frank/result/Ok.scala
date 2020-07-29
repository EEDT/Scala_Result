package com.frank.result
/**
 * @constructor 用x创建一个Ok
 * @param x 该Ok所包含的值
 * @tparam T x的类型
 */
case class Ok[T,E](private val x:T) extends AnyVal with Result[T,E]  {
  /**
   * Type of this.x
   * for example
   * {{{
   *   override def map[U](f: E => U):M[U] = Err(f(x))
   * }}}
   */
  override type TypeOf = T

  /**
   * 同[[scala.util.Either]]中的map方法
   * @param f 函数
   * @tparam U 返回
   * @return result
   */
  override def map[U](f: T => U): Result[U, E] = Ok(f(x))

  /**
   * 如果该result为ok且f(x)为true，返回true
   * 否则返回false
   * @param f 函数
   * @return
   */
  def exists(f: T => Boolean):Boolean = f(x)

  /**
   * 对于该result所包含的值执行f()
   * @param f 函数
   */
  def foreach(f: T => Unit): Unit = f(x)

  /**
   * 将该result转为seq后flatmap
   */
  def flatMap[U](f: T => IterableOnce[T]):Seq[TypeOf] = {
    this.toSeq.flatMap(f)
  }
  /**
   * 创建seq
   * @return seq
   */
  def toSeq: Seq[T] = Seq(x)

  /**
   * 将该result转为seq后flatten
   * @param f 函数
   * @return seq
   */
  def flatten(implicit f: T => IterableOnce[T]): Seq[T] =
    this.toSeq.flatten(f)

  /**
   * 返回迭代器
   * @return iterator
   */
  def iterator(): Iterator[T] = Iterator(x)
}
