package Euler9;

public class Euler9 {
    public static void main(String[] args) {
        System.out.println(find_triplet());
    }

    private static int find_triplet() {
        int result = 0;
        for (int a = 0; a < 1000; a++) {
            if(result != 0){
                break;
            }
            for (int b = a + 1; b < 1000; b++) {
                int c = 1000 - a - b;
                if ((Math.pow(a, 2) + Math.pow(b, 2)) == Math.pow(c, 2)) {
                    result = a * b * c;
                }
            }
        }
        return result;
    }
}
