package com.frank.result
/**
 * @constructor 用x创建一个Ok
 * @param x 该Ok所包含的值
 * @tparam T x的类型
 */
case class Ok[T,E](x:T) extends Result[T,E]
