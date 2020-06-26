package com.frank.result

/**
 * @constructor 用x创建一个Err
 * @param x 该Err所包含的值
 * @tparam E x的类型
 */
case class Err[T,E](x:E) extends Result[T,E]
