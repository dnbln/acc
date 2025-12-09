int foo(int a, int b){
    int x;
    int y;
    if(a<0) {
        x=1;
    } else {
        if (a>0) {
            x=2;
        }
    }
    if(b>0) {
        y=x;
    } else {
        y=0;
    }
    return y;
}