int fibo (int n) {
  if (n == 0) return 0;
  if (n == 1) return 1;
  return fibo(n-1) + fibo(n-2);
}

int fact (int n) {
  if (n == 0) return 1; 
  return fact(n-1) * n;
}

int main () {
  int a;
  a = fibo(fibo(fibo(5)));  println(a);
  a = fibo(fibo(6));        println(a);
  a = fact(5);              println(a);
  a = fibo(15);             println(a);
  a = fibo(30);             println(a);

  // all this repeated call should not consume stack space.
  fibo(10); fibo(10); fibo(10); fibo(10); fibo(10); fibo(10); fibo(10);

  return 0;
}
