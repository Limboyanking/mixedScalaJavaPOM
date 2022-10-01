package learn;

import java.util.Scanner;

public class Addition {
    public static void main(String args[]){
        System.out.println("Hello Java");

        int number1;
        int number2 = 5;
        int sum = 0;

        sum = number2 + 10;
        System.out.println("The sum of 10 and " + number2 + " is " + sum);

        Scanner input = new Scanner(System.in);

        System.out.println("Enter number 1 : ");
        number1 = input.nextInt();

        System.out.println("Enter number 2 : ");
        number2 = input.nextInt();

//        input.next(); // String

//        int x = Integer.parseInt("123.4");

        sum = number1 + number2;
        System.out.println("The sum of " + number1 + " and " + number2 + " is " + sum);

    }
}
