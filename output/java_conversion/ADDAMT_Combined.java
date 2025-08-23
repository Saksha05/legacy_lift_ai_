// Combined Java conversion from COBOL program
// Original source: output/ADDAMT_ast.json
// API Provider: groq


// ===== Identification Division =====
public class IDENTIFICATIONDIVISION {
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

