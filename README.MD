# Scala Result

一个用Scala实现Rust的`std::Result<T, E>`的工具。

注：标准库里已经有了`scala.util.Either`了。但是本项目对于Either进行了提升（Rust有的这个都有。Either没有`unwrap`，各种`xx_or_else`哦。）

## 安装

下载 [result.jar](https://github.com/EEDT/Scala_Result/releases/download/2.5.1/Scala_Result.jar)，然后把他放在一个你喜欢的地方，我们把它叫做`path_of_reslt_jar`。

为什么没有maven repo或者GitHub package呢？这样就够用了，~~不依赖第三方哦~~。

然后在你的gradle项目的`build.gradle`中加入

```groovy
implementation file("path_of_result_jar/scala_result.jar")
```

## 文档

<https://eedt.github.io/result_docs/>

## 例子

```scala
package test
import com.frank.result._

object Test extends App {
  def saveDiv(a: Int, b: Int): Result[Int, String] =
    try Ok(a / b)
    catch {
      case _: Exception => Err("divide by zero")
    }
  val res =
    for {
      a <- saveDiv(6, 3)
      b <- saveDiv(6, 2)
    } yield a + b
  println(result.unwrap) //prints 5
}
```
