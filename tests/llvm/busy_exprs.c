//@ARGS: --cfg --llvm-ir --llvm-optimized-ir
int main(int a, int b) {
    int x = a + b;
    int y = a * b;
    if (a < b) {
        x = a - b;
    } else {
        y = a - b;
    }
    return x * y;
}
