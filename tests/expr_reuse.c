//@ARGS: --cfg
int foo(int a, int b, int c){
    int x = 0;
    int y = 0;
    int s = b+c;
    int z;
    if(a+b<0) {
        x=s;
    }
    if (a+b>0){
        y=b+c;
        b=2;
    }
    z= b+c;
    return y;
}