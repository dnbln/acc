//@ARGS: --cfg --llvm-ir --llvm-optimized-ir
int factorial(int n) {
  if (n <= 1) {
    return 1;
  }
  return n * factorial(n - 1);
}