int avg (int count, int* value) {
  int total, i; 
  total = 0;
  for (i = 1; i < count; i++) {
    total = total + value[i];
  }
  return (total / count);
}

int main (void) {
  int studentNumber, count, i, sum;
  int mark[4];
  float average;

  /* now support comments */
  // and this

  count = 4;
  sum = 0;
  for (i = 1; i < count; i++) {
    mark[i] = i * 30;
    sum = sum + mark[i];
    average = avg(i+1, mark);
    if (average > 40) {
      printf("%f", average);
    }
  }
  if (average > 40) printf("%f", average);
  if (average > 40) printf("%f", average);
}

  /* now support comments and not greedy */
