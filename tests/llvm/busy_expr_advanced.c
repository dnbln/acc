//@ARGS: --cfg --llvm-ir --llvm-optimized-ir --opt-debug hp
int f(int a, int b) {
    int x = a + b;
    int y = a * b;
    int z = a / b;
    if (a < b) {
        x = a - b;
    } else {
        if (a > b) {
            z = a / b;
        } else {
        }
        y = a - b;
    }
    return x * y + z;
}