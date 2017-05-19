object Fib{
  def main(args: Array[String]) = {
    println(fib(33)())
    printf("%d KB\n",Runtime.getRuntime().totalMemory()/4/1024);
  }

  def fib(x: Int): () => Int = {
    if (x<2) return {() => x}
    var f1 = fib(x-1)
    var f2 = fib(x-2)
    return {() => f1() + f2()}
  }
}
