# Scala Result

## 简介

一个用Scala实现rust的result的项目，`Result<T,E>`是rust的一个很实用的工具，你可以这样:

```Rust
fn main(){
  let x = Square::creat_square(1, 1).unwrap();
  println!("{:?}",x);
}
#[derive(Debug)]
struct Square{
  x:i32,
  y:i32
}
impl Square{
  fn zero()->Square{
    return Square{
      x:0,
      y:0
    };
  }
  pub fn creat_square(xs:i32,ys:i32)->Result<Square,Square>{
    return if xs == ys {
      Ok(Square{x:xs,y:ys})
    }else{
      Err(Square::zero())
    }
  }
}
```

使用此工具你可以这样

```scala
import com.frank.result
class Square private(val x:Int,val y:Int)
object Square{
  def apply(x:Int,y:Int):result.Result[Square,Square] = {
    if(x == y){
      result.Ok(new Square(x,y))
    }else{
      result.Err(new Square(0,0))
    }
  }
}
```

虽然scala提供了`scala.util.Either`，`scala.util.Right`，`scala.util.Left`，但是这个项目提供了`exception`，`unwrap`，`ok`，`err`等实用功能

## 安装

### 1.`clone`本项目

```shell script
git clone https://github.com/EEDT/Scala_Result.git
gradle build
```

## 使用

从版本页面下载jar，把jar放到任何一个地方，然后在`build.gradle`中加入

```groovy
implementation files 'path_of_result_jar/result.jar'
```

- `path_of_result_jar`:放jar的路径
