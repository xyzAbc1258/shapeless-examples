package org.example

//Provider of value V, tagged additionally with type K
//This tag, can be used to resolve implicits
trait TaggedValue[K, V] {
  def value: V
}

object TaggedValue {
  //Simple helper method which returns value with tag matching `provider` argument
  def value[A](provider: Any)(implicit v: TaggedValue[provider.type, A]): A = v.value
}

//Simple helper class for objects defining tagged values, tagged with it's own type
trait TaggedValueProvider {
  type Value[A] = TaggedValue[this.type, A]
  def value[A](v: A): Value[A] = new TaggedValue[this.type, A] {
    override def value: A = v
  }
}

object SomeProvider extends TaggedValueProvider {
  implicit val int: Value[Int] = value(12)
}

object AnotherProvider extends TaggedValueProvider {
  implicit val int: Value[Int] = value(42)
}

object Test {
  def main(args: Array[String]): Unit = {
    println("SomeProvider tagged Int: " + TaggedValue.value[Int](SomeProvider))
    println("AnotherProvider tagged Int: " + TaggedValue.value[Int](AnotherProvider))
  }
}
