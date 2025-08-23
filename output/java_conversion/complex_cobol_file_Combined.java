// Combined Java conversion from COBOL program
// Original source: output/complex_cobol_file_ast.json
// API Provider: groq


// ===== Identification Division =====
public class COMPLEXSAMPLE {
}


// ===== Environment Division =====
import java.io.*;
import java.nio.file.*;

public class FileConfig {
    private static final String INPUT_FILE = "actual_filename";
    private static final String OUTPUT_FILE = "actual_filename";
}


// ===== Data Division =====
public class DataStructure {
    private String studentId;
    private String studentName;
    private String courseCode;
    private int score;
    private String[] studentTable;
    private int count;
    private double average;
    private boolean endOfFile;
    private boolean eof;

    public DataStructure() {
        this.studentId = "";
        this.studentName = "";
        this.courseCode = "";
        this.score = 0;
        this.studentTable = new String[10];
        this.count = 0;
        this.average = 0.0;
        this.endOfFile = false;
        this.eof = false;
    }

    public void setStudentId(String value) {
        if (value != null && value.length() <= 5) {
            this.studentId = value;
        } else {
            throw new IllegalArgumentException("Student ID must be 5 characters or less");
        }
    }

    public void setStudentName(String value) {
        if (value != null && value.length() <= 20) {
            this.studentName = value;
        } else {
            throw new IllegalArgumentException("Student name must be 20 characters or less");
        }
    }

    public void setCourseCode(String value) {
        if (value != null && value.length() <= 5) {
            this.courseCode = value;
        } else {
            throw new IllegalArgumentException("Course code must be 5 characters or less");
        }
    }

    public void setScore(int value) {
        if (value >= 0 && value <= 999) {
            this.score = value;
        } else {
            throw new IllegalArgumentException("Score must be between 0 and 999");
        }
    }

    public void setCount(int value) {
        if (value >= 0) {
            this.count = value;
        } else {
            throw new IllegalArgumentException("Count must be 0 or greater");
        }
    }

    public void setAverage(double value) {
        if (value >= 0.0) {
            this.average = value;
        } else {
            throw new IllegalArgumentException("Average must be 0.0 or greater");
        }
    }

    public void setEndOfFile(boolean value) {
        this.endOfFile = value;
    }

    public void setEof(boolean value) {
        this.eof = value;
    }
}

