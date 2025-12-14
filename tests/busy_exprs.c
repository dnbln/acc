//@ARGS: --cfg
int main(int a, int b) {
    int x = a + b;
    int y = a * b;
    if (1) {
        x = a - b;
    } else {
        y = a - b;
    }
    return x + y;
}
