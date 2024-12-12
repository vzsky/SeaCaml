
int main () {
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
  // println(b); // should error
}

// response should be 
// 3 3 10 2 0
