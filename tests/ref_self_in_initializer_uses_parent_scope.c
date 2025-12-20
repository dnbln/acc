//@ARGS: --resolve --cfg
int main() {
    int x = 0;
    {
        int x = x;
    }

    return x;
}