int main() {
  int array[6][3];

  println(array);

  array[0][0] = 0;
  array[0][1] = 1;
  array[0][2] = 2;

  array[1][0] = 3;
  array[1][1] = 4;
  array[1][2] = 5;

  array[2][0] = 6;
  array[2][1] = 7;
  array[2][2] = 8;

  array[3][0] = 9;
  array[3][1] = 0;
  array[3][2] = 1;

  array[4][0] = 2;
  array[4][1] = 3;
  array[4][2] = 4;

  array[5][0] = 5;
  array[5][1] = 6;
  array[5][2] = 7;

  println(array);

  return 0;
}
