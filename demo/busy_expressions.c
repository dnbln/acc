//@ARGS: --cfg --opt-debug hp
//To run: cargo run demo/busy_expressions.c --cfg --opt-debug hp
int foo(int a, int b, int t, int u){
    int x=a+b;
    int y=a*b;
    if(t<u) {
        x=a-b;
    }else{
        y=a-b;
    }
    x=x*y;
    return x;
}