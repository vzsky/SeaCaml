int fibo (int n) {
  if (n == 0) return 0;
  if (n == 1) { return 1; }
  return fibo(n-2) + fibo(n-1);
}

int main () {
  int f[30], f2[30];
  int i;
  int n;

  n = 5;

  for (i = 0; i < n; i++) f[i] = fibo(i);

  f2[0] = 0; f2[1] = 1;
  for (i = 2; i < n; i++) { f2[i] = f2[i-1] + f2[i-2]; }

  int correct; 
  correct = 0;

  for (i = n-1; i >= 0; i = i - 1) {
    if (f[i] == f2[i]) correct++;
  }

  if (correct == n) { 
    println("hooray"); 
  }

  return 0;
}
