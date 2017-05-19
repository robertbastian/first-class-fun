function fib(x) {
  if (x<2) return () => x
  var f1 = fib(x-1)
  var f2 = fib(x-2)
  return () => f1() + f2()
}

console.log(fib(33)())
console.log(process.memoryUsage())
