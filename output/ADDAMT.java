import java.util.Scanner;

public class ADDAMT {

    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        String custNoIn;
        int amt1In, amt2In, amt3In, totalOut;
        String moreData = "YES";

        do {
            System.out.println("ENTER NAME       (15 CHARACTERS)");
            custNoIn = input.nextLine();
            System.out.println("Enter amount of first purchase (5 digits)");
            amt1In = input.nextInt();
            System.out.println("Enter amount of second purchase (5 digits)");
            amt2In = input.nextInt();
            System.out.println("Enter amount of third purchase (5 digits)");
            amt3In = input.nextInt();
            input.nextLine(); // Consume newline

            totalOut = amt1In + amt2In + amt3In;
            System.out.println(custNoIn + " Total Amount = " + totalOut);
            System.out.println("MORE INPUT DATA (YES/NO)?");
            moreData = input.nextLine().toUpperCase();
            if(moreData.startsWith("NO")){
                moreData = "NO ";
            }

        } while (!moreData.equals("NO "));
        input.close();
    }
}