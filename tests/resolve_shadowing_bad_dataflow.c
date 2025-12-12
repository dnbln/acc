//@FAIL
int main() {
    int x;
    {
        int x = 5;
        {
            x = 10;
        }
    }

    return x;
}