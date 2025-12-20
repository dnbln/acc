//@ARGS: --cfg --llvm-ir --llvm-optimized-ir --optimizations vips,cp,dve,bi,dbg,dbgv,hp,vips,phi2sel,bd,tu,dve,tdb,bi
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
