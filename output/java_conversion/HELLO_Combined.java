// Combined Java conversion from COBOL program
// Original source: output/HELLO_ast.json
// API Provider: gemini


// ===== Identification Division =====
package com.cobol.converted;

public class HELLO {
    public HELLO() {
        //Constructor
    }

    public static void main(String[] args) {
        //Main method -  Equivalent to the COBOL program's execution point.  No inherent COBOL logic to convert.
    }
}


// ===== Procedure Division =====
import java.util.Scanner;
import java.util.InputMismatchException;

class DataStructure {
    private String custNoIn;
    private int amt1In;
    private int amt2In;
    private int amt3In;
    private String custNoOut;
    private int totalOut;
    private String moreData;

    public void setCustNoIn(String custNoIn) {
        if (custNoIn.length() > 15) {
            throw new IllegalArgumentException("Customer number exceeds 15 characters");
        }
        this.custNoIn = custNoIn;
    }

    public String getCustNoIn() {
        return custNoIn;
    }

    public void setAmt1In(int amt1In) {
        if (amt1In < 0 || amt1In > 99999) {
            throw new IllegalArgumentException("Amount 1 must be between 0 and 99999");
        }
        this.amt1In = amt1In;
    }

    public int getAmt1In() {
        return amt1In;
    }

    public void setAmt2In(int amt2In) { this.amt2In = amt2In;}
    public int getAmt2In() { return amt2In;}

    public void setAmt3In(int amt3In) { this.amt3In = amt3In;}
    public int getAmt3In() { return amt3In;}


    public void setCustNoOut(String custNoOut) {
        this.custNoOut = custNoOut;
    }

    public String getCustNoOut() {
        return custNoOut;
    }

    public void setTotalOut(int totalOut) {
        this.totalOut = totalOut;
    }

    public int getTotalOut() {
        return totalOut;
    }

    public void setMoreData(String moreData) {
        this.moreData = moreData;
    }

    public String getMoreData() {
        return moreData;
    }
}

public class CobolToJava {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        DataStructure data = new DataStructure();

        try {
            data.setAmt2In(0);
            data.setAmt3In(0);
            data.setMoreData("YES");
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
                    scanner.nextLine(); 
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
                scanner.nextLine(); 
            }
        } catch (Exception e) {
            System.err.println("Unexpected error: " + e.getMessage());
        } finally {
            scanner.close();
        }
    }
}

