int main() {
    int a, b, x;
    int c, d, e;
    a = 0;
    b = 1;
    x = a+b;

    if (a < b) c = 2;
    if (x > a) {
      c = 3;
    }
    if (a > 1) d = 2;
    if (x == b) {
      d = 5;
      e = 6;
    }

    return 0;
}
