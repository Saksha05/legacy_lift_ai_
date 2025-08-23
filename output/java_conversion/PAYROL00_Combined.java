// Combined Java conversion from COBOL program
// Original source: output/PAYROL00_ast.json
// API Provider: groq


// ===== Identification Division =====
public class Payrol00 {
}


// ===== Data Division =====
public class DataStructure {
    private String custNoIn;     // PIC X(15)
    private int amt1In;          // PIC 9(5) 
    private int amt2In;          // PIC 9(5)
    private int amt3In;          // PIC 9(5)
    private String custNoOut;    // PIC X(15)
    private int totalOut;        // PIC 9(6)
    private String moreData = "YES"; // PIC X(3) VALUE 'YES'
    public void setCustNoIn(String value) {
        if (value != null && value.length() <= 15) {
            this.custNoIn = value;
        } else {
            throw new IllegalArgumentException("Customer number must be 15 characters or less");
        }
    }
    public void setAmt1In(int value) {
        if (value >= 0 && value <= 99999) {
            this.amt1In = value;
        } else {
            throw new IllegalArgumentException("Amount must be between 0 and 99999");
        }
    }
    public DataStructure() {
        this.custNoIn = "";
        this.amt1In = 0;
        this.amt2In = 0;
        this.amt3In = 0;
        this.custNoOut = "";
        this.totalOut = 0;
    }
}


// ===== Procedure Division =====
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Scanner;
import java.util.InputMismatchException;
public class DataStructure {
    private String custNoIn;
    private int amt1In;
    private int amt2In;
    private int amt3In;
    private int totalOut;
    private String moreData;
    public void setCustNoIn(String custNoIn) {
        if (custNoIn.length() > 15) {
            throw new IllegalArgumentException("Error: CustNoIn exceeds 15 characters");
        }
        this.custNoIn = custNoIn;
    }
    public void setAmt1In(int amt1In) {
        this.amt1In = amt1In;
    }
    public void setAmt2In(int amt2In) {
        this.amt2In = amt2In;
    }
    public void setAmt3In(int amt3In) {
        this.amt3In = amt3In;
    }
    public void setTotalOut(int totalOut) {
        if (totalOut > 999999) {
            System.err.println("Warning: Total exceeds maximum value");
        }
        this.totalOut = totalOut;
    }
    public void setMoreData(String moreData) {
        this.moreData = moreData;
    }
    public String getCustNoIn() {
        return custNoIn;
    }
    public int getAmt1In() {
        return amt1In;
    }
    public int getAmt2In() {
        return amt2In;
    }
    public int getAmt3In() {
        return amt3In;
    }
    public int getTotalOut() {
        return totalOut;
    }
    public String getMoreData() {
        return moreData;
    }
}
public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        DataStructure data = new DataStructure();
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
             PrintWriter writer = new PrintWriter("output.txt")) {
            while (!data.getMoreData().equals("NO")) {
                System.out.print("ENTER NAME (15 CHARACTERS): ");
                String custNo = scanner.nextLine();
                try {
                    data.setCustNoIn(custNo);
                } catch (IllegalArgumentException e) {
                    System.err.println("Error: " + e.getMessage());
                    continue;
                }
                System.out.print("Enter amount of first purchase (5 digits): ");
                try {
                    int amt1 = scanner.nextInt();
                    data.setAmt1In(amt1);
                } catch (InputMismatchException e) {
                    System.err.println("Error: Please enter a valid number");
                    scanner.nextLine(); // Clear invalid input
                    continue;
                } catch (IllegalArgumentException e) {
                    System.err.println("Error: " + e.getMessage());
                    continue;
                }
                data.setCustNoOut(data.getCustNoIn());
                int total = data.getAmt1In() + data.getAmt2In() + data.getAmt3In();
                if (total > 999999) {
                    System.err.println("Warning: Total exceeds maximum value");
                }
                data.setTotalOut(total);
                System.out.println(data.getCustNoOut() + " Total Amount = " + data.getTotalOut());
                System.out.print("MORE INPUT DATA (YES/NO)? ");
                String moreInput = scanner.next().toUpperCase();
                data.setMoreData(moreInput.startsWith("Y") ? "YES" : "NO");
                scanner.nextLine(); // Clear buffer
            }
        } catch (IOException e) {
            System.err.println("Error: " + e.getMessage());
        }
    }
}

