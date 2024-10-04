package Euler22;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class Euler22 {

    public static void main(String[] args) throws FileNotFoundException {
        Scanner scanner = new Scanner(new File("/Users/arslanefimov/IdeaProjects/functional_programming/lab1_fp/src/euler_22/names.txt"));
        int sum = 0;
        while(scanner.hasNextLine()){
            String result = scanner.nextLine().replace("\"", "");
            String[] array = result.split(",");
            List<String> list = Arrays.stream(array).toList().stream().sorted().toList();
            for(int i = 0; i < list.size(); i++){
                int tmp = 0;
                for(char chr : list.get(i).toCharArray()){
                    tmp += getCharAlphabetNumber(chr);
                }
                tmp = tmp * (i+1);
                sum += tmp;
            }
        }
        System.out.println(sum);
        scanner.close();
    }

    private static int getCharAlphabetNumber(char character){
        if(character >= 'A' && character <= 'Z'){
            return (character - 'A') + 1;
        }
        return 0;
    }
}
