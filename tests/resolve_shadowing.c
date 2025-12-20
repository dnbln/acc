//@ARGS: --resolve --cfg
int main() {
    int x = 0;
    {
        int x = 5;
        {
            x = 10;
        }
    }

    return x;
}