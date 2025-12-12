//@ARGS: --cfg
int factorial(int n) {
  if (n <= 1) {
    return 1;
    factorial(n + 1); // this statement is unreachable
  }
  return n * factorial(n - 1);
}