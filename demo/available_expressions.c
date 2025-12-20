//@ARGS: --cfg --opt vips,hp --opt-debug hp
//To run: cargo run demo/available_expressions.c --cfg --opt vips,hp --opt-debug hp
int f(int a, int b, int c){
    int x=0;
    int y=0;
    int z;
    int s= b+c;
    if(a+b<0) {
        x=s;
    }
    if (a+b>0){
        y=c+b;
        b=2;
    }
    z = b+c;
    return z;
}