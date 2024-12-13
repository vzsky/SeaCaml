int f () {
  // println(c); // should error
  int x = 0;
  println(x);
}

int main () {
  int c = 10101;
  int a = 0;
  if (0 == 0) {
    int a = 1;
    int b = 2;
    a = 3;
    println(a);
    if (0 == 0) {
      int b = 3;
      a = 10;
      println(b);
    }
    println(a);
    println(b);
  }
  println(a);

  f();
  // println(b); // should error
}

// response should be 
// 3 3 10 2 0
