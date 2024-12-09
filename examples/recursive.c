void test (int n) {
  if (n == 0) return; 
  println(n);
}

int fibo (int n) {
  if (n == 0) return 0;
  if (n == 1) return 1;
  return fibo(n-1) + fibo(n-2);
}

int main () {
  int a, b, c, d, e;
  test (99);
  test (0);
  a = fibo(0);
  b = fibo(1);
  c = fibo(2);
  d = fibo(3);
  d = fibo(10);
  // all this repeated call should not consume stack space.
  e = fibo(10);
  e = fibo(10);
  e = fibo(10);
  e = fibo(10);
  e = fibo(10);

  println(d);
  return 0;
}
