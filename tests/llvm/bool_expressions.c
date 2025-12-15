//@ARGS: --cfg --llvm-ir --llvm-optimized-ir
bool f(int a, int b) {
    return (a > b) && (a != b || b > 0);
}
