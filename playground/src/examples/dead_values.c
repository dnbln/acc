//@ARGS: --cfg --opt-debug cpdvetdb
//To run: cargo run demo/dead_values.c --cfg --opt-debug cpdvetdb
int foo(int a, int b){
    int x = a + b;
    int y = a - b;
    int z = a * b;
    int w = a / b;

    if (a > b) {
        x = x + 1;
    } else {
        y = y - 1;
    }

    return x;
}
