class Fib {
public static void main(String args[]) {
  for (Integer n = 0; n < 10; n ++) {
    System.out.println("fib(" + n + ") = " + fib(n));
  }
}

public static Integer fib(Integer n) {
  if (n <= 1) {
    return 1;
  }
  return fib(n - 1) + fib(n - 2);
}

}
