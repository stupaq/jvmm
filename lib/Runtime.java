import java.util.Scanner;

public class Runtime {
  private static final Scanner stdin = new Scanner(System.in);

  public static String concat(String a, String b) {
    return a + b;
  }

  public static void printInt(int a) {
    isNotNull(a);
    System.out.println(a);
  }

  public static void printString(String a) {
    isNotNull(a);
    System.out.println(a);
  }

  public static void error() {
    System.out.println("runtime error");
    System.exit(-1);
  }

  public static int readInt() {
    int n = stdin.nextInt();
    stdin.nextLine();
    return n;
  }

  public static String readString() {
    return stdin.nextLine();
  }

  private static void isNotNull(Object object) {
    if (object == null) {
      throw new NullPointerException("Notion of nullity is a total disaster, kill it with fire!");
    }
  }
}
