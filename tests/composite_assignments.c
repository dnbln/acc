//@ARGS: --cfg
int main() {
    int x;
    if (1) {
        x = 10;
    } else {
        x = 20;
    }
    x += 2;
    x -= 3;
    x *= 4;
    x /= 5;
    x %= 6;
    return x;
}