int foo () {
  return 5;
}

int bar (int x) {
  return x + 1;
}

int baz (int z) {
  return z + bar(z);
}

int main () {
  int a, b, c;
  a = foo();
  b = bar(a);
  c = baz(foo());

  println(a);
  println(b);
  println(c);
  return 0;
}
