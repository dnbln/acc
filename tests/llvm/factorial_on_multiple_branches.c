//@ARGS: --cfg --llvm-ir --llvm-optimized-ir --opt-debug hp,phi2sel,bd,bi
int factorial(int n) {
  if (n <= 1) {
    return 1;
  }
  return n * factorial(n - 1);
}

int f(int x) {
  int result;

  if (x < 10) {
    result = 2;
  } else {
    result = 3;
  }

  for (int i = 0; i < 3; i++) {
    result += i;
  }

  if (x < 10) {
    return factorial(result);
  } else {
    return factorial(result);
  }
}

int main() {
  return f(5);
}