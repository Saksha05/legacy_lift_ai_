#!/usr/bin/env python3
"""
Free API Converter - Converts COBOL AST chunks to Java using free LLM APIs
"""

import json
import os
import requests
import urllib3
from pathlib import Path
from typing import Dict, List, Any, Optional

# Try to load environment variables from .env file
try:
    from dotenv import load_dotenv
    load_dotenv()
except ImportError:
    # dotenv not installed, skip loading from .env file
    pass

# Disable SSL warnings
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

class FreeAPIConverter:
    def __init__(self, api_provider: str = "huggingface", api_key: str = None):
        self.api_provider = api_provider.lower()
        # Get API key from environment variable if not provided
        self.api_key = api_key or os.getenv(f"{self.api_provider.upper()}_API_KEY")
        self.output_dir = Path("output/java_conversion")
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # API configurations
        self.api_configs = {
            "huggingface": {
                "url": "https://api-inference.huggingface.co/models/microsoft/DialoGPT-medium",
                "headers": {"Authorization": f"Bearer {self.api_key}"} if self.api_key else {},
                "model": "microsoft/DialoGPT-medium"
            },
            "gemini": {
                "url": "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent",
                "headers": {"Content-Type": "application/json"} if self.api_key else {},
                "model": "gemini-1.5-flash"
            },
            "together": {
                "url": "https://api.together.xyz/v1/chat/completions",
                "headers": {"Authorization": f"Bearer {self.api_key}", "Content-Type": "application/json"} if self.api_key else {},
                "model": "meta-llama/Llama-2-7b-chat-hf"
            },
            "openrouter": {
                "url": "https://openrouter.ai/api/v1/chat/completions",
                "headers": {"Authorization": f"Bearer {self.api_key}", "Content-Type": "application/json"} if self.api_key else {},
                "model": "microsoft/wizardlm-2-8x22b"
            }
        }
    
    def create_conversion_prompt(self, division_data: Dict[str, Any], division_type: str) -> str:
        """Create a specialized prompt for each division type with detailed mapping rules"""
        
        base_prompt = f"""You are an expert COBOL to Java converter. Convert the following COBOL {division_type.replace('_', ' ').title()} AST to functionally equivalent Java code.

COBOL Division: {division_type}
AST: {json.dumps(division_data['ast_node'], indent=2)}

CRITICAL REQUIREMENTS:
1. Generate ONLY executable Java code - no explanations or markdown
2. Ensure functional equivalence to original COBOL logic
3. Use modern Java practices (try-with-resources, proper exception handling)
4. Create a single cohesive class structure

SPECIFIC CONVERSION RULES:
"""
        
        if division_type == "identification_division":
            prompt = base_prompt + """
- Extract PROGRAM-ID and create main Java class with that name
- Add package declaration: package com.cobol.converted;
- Create public class with proper constructor
- Add class-level documentation from COBOL comments
- Example: PROGRAM-ID. HELLO → public class Hello

REQUIRED OUTPUT FORMAT:
```java
package com.cobol.converted;

public class [ProgramName] {
    // Class implementation
}
```
"""
        
        elif division_type == "environment_division":
            prompt = base_prompt + """
- Convert SELECT statements to File objects with exact file names
- Map ASSIGN TO 'filename' → new File("filename")
- Add all necessary imports for file I/O
- Create static final constants for file paths
- Example: SELECT INPUT-FILE ASSIGN TO 'INPUT.TXT' → private static final String INPUT_FILE = "INPUT.TXT";

REQUIRED IMPORTS:
import java.io.*;
import java.nio.file.*;

REQUIRED OUTPUT FORMAT:
```java
import java.io.*;
import java.nio.file.*;

public class FileConfig {
    private static final String INPUT_FILE = "actual_filename";
    private static final String OUTPUT_FILE = "actual_filename";
}
```
"""
        
        elif division_type == "data_division":
            prompt = base_prompt + """
- Convert PIC X(n) → String (with max length n validation)
- Convert PIC 9(n) → int or long based on size (with range validation)
- Convert PIC 9(n)V9(m) → double or BigDecimal (with precision validation)
- Convert 01 level → class fields with proper encapsulation
- Convert 05 level → nested class or simple field with validation
- Convert VALUE clauses → field initialization with type safety
- Handle OCCURS n TIMES → arrays of size n with bounds checking

EXACT MAPPINGS WITH VALIDATION:
- PIC X → String (validate non-null)
- PIC X(15) → String (validate max 15 chars, add validation method)
- PIC 9(3) → int (validate range 0-999)
- PIC 9(5) → int (validate range 0-99999)
- PIC 9(9) → long (validate range)
- VALUE 'YES' → = "YES" (with enum or constant validation)
- VALUE 0 → = 0 (with range validation)

REQUIRED OUTPUT FORMAT WITH VALIDATION:
```java
public class DataStructure {
    private String custNoIn;     // PIC X(15)
    private int amt1In;          // PIC 9(5) 
    private int amt2In;          // PIC 9(5)
    private int amt3In;          // PIC 9(5)
    private String custNoOut;    // PIC X(15)
    private int totalOut;        // PIC 9(6)
    private String moreData = "YES"; // PIC X(3) VALUE 'YES'
    
    // Validation methods
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
    
    // Constructor with validation
    public DataStructure() {
        this.custNoIn = "";
        this.amt1In = 0;
        this.amt2In = 0;
        this.amt3In = 0;
        this.custNoOut = "";
        this.totalOut = 0;
    }
}
```
"""
        
        elif division_type == "procedure_division":
            prompt = base_prompt + """
- Convert OPEN INPUT/OUTPUT → BufferedReader/PrintWriter with validation
- Convert READ → reader.readLine() with null check and error handling
- Convert WRITE → writer.println() with validation
- Convert MOVE A TO B → B = A with type validation
- Convert PERFORM UNTIL → while loop with proper condition handling
- Convert ACCEPT → Scanner input with validation and error handling
- Convert DISPLAY → System.out.println() with formatting
- Convert ADD/SUBTRACT → arithmetic with overflow checking
- Convert INSPECT → String manipulation with validation
- Convert CLOSE → proper resource cleanup

EXACT LOGIC MAPPINGS WITH VALIDATION:
- ACCEPT field → Scanner input with type validation and retry logic
- DISPLAY 'prompt' → System.out.print("prompt: ")
- MOVE field1 TO field2 → field2 = field1 (with validation using setter methods)
- ADD A B C GIVING D → D = A + B + C (with overflow checking)
- PERFORM UNTIL condition → while (!condition) with proper boolean handling
- INSPECT field CONVERTING 'abc' TO 'ABC' → field = field.toUpperCase()

REQUIRED OUTPUT FORMAT WITH VALIDATION:
```java
import java.util.Scanner;
import java.util.InputMismatchException;

public static void main(String[] args) {
    Scanner scanner = new Scanner(System.in);
    DataStructure data = new DataStructure();
    
    try {
        while (!data.getMoreData().equals("NO")) {
            // Input with validation
            System.out.print("ENTER NAME (15 CHARACTERS): ");
            String custNo = scanner.nextLine();
            try {
                data.setCustNoIn(custNo);
            } catch (IllegalArgumentException e) {
                System.err.println("Error: " + e.getMessage());
                continue;
            }
            
            // Numeric input with validation
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
            
            // Process data with validation
            data.setCustNoOut(data.getCustNoIn());
            int total = data.getAmt1In() + data.getAmt2In() + data.getAmt3In();
            
            // Check for overflow
            if (total > 999999) {
                System.err.println("Warning: Total exceeds maximum value");
            }
            data.setTotalOut(total);
            
            System.out.println(data.getCustNoOut() + " Total Amount = " + data.getTotalOut());
            
            // Continue prompt with validation
            System.out.print("MORE INPUT DATA (YES/NO)? ");
            String moreInput = scanner.next().toUpperCase();
            data.setMoreData(moreInput.startsWith("Y") ? "YES" : "NO");
            scanner.nextLine(); // Clear buffer
        }
    } catch (Exception e) {
        System.err.println("Unexpected error: " + e.getMessage());
    } finally {
        scanner.close();
    }
}
```
"""
        
        prompt += "\n\nIMPORTANT: Output ONLY the Java code block. No explanations, no markdown formatting, no additional text."
        return prompt
    
    def call_huggingface_api(self, prompt: str) -> Optional[str]:
        """Call Hugging Face Inference API"""
        config = self.api_configs["huggingface"]
        
        payload = {"inputs": prompt}
        
        try:
            response = requests.post(
                config["url"],
                headers=config["headers"],
                json=payload,
                timeout=60
            )
            
            if response.status_code == 200:
                result = response.json()
                if isinstance(result, list) and len(result) > 0:
                    return result[0].get("generated_text", "")
                return str(result)
            else:
                print(f"Hugging Face API error: {response.status_code}")
                return None
                
        except Exception as e:
            print(f"Hugging Face API error: {e}")
            return None
    
    
    def call_together_api(self, prompt: str) -> Optional[str]:
        """Call Together AI API"""
        config = self.api_configs["together"]
        
        payload = {
            "model": config["model"],
            "messages": [
                {"role": "system", "content": "You are a COBOL to Java conversion expert."},
                {"role": "user", "content": prompt}
            ],
            "temperature": 0.1,
            "max_tokens": 2000
        }
        
        try:
            response = requests.post(
                config["url"],
                headers=config["headers"],
                json=payload,
                timeout=60
            )
            
            if response.status_code == 200:
                result = response.json()
                return result["choices"][0]["message"]["content"]
            else:
                print(f"Together AI API error: {response.status_code}")
                return None
                
        except Exception as e:
            print(f"Together AI error: {e}")
            return None
    
    def call_openrouter_api(self, prompt: str) -> Optional[str]:
        """Call OpenRouter API"""
        config = self.api_configs["openrouter"]
        
        payload = {
            "model": config["model"],
            "messages": [
                {"role": "system", "content": "You are a COBOL to Java conversion expert."},
                {"role": "user", "content": prompt}
            ],
            "temperature": 0.1,
            "max_tokens": 2000
        }
        
        try:
            response = requests.post(
                config["url"],
                headers=config["headers"],
                json=payload,
                timeout=60
            )
            
            if response.status_code == 200:
                result = response.json()
                return result["choices"][0]["message"]["content"]
            else:
                print(f"OpenRouter API error: {response.status_code}")
                return None
                
        except Exception as e:
            print(f"OpenRouter error: {e}")
            return None

    def call_gemini_api(self, prompt: str) -> Optional[str]:
        """Call Google Gemini API"""
        config = self.api_configs["gemini"]
        url = f"{config['url']}?key={self.api_key}"
        payload = {
            "contents": [{
                "parts": [{
                    "text": prompt
                }]
            }]
        }
        try:
            response = requests.post(
                url,
                headers=config["headers"],
                json=payload,
                timeout=60,
                verify=False
            )
            if response.status_code == 200:
                result = response.json()
                if "candidates" in result and len(result["candidates"]) > 0:
                    return result["candidates"][0]["content"]["parts"][0]["text"]
                return None
            else:
                print(f"Gemini API error: {response.status_code}")
                print(f"Response: {response.text}")
                return None
        except Exception as e:
            print(f"Gemini error: {e}")
            return None
    
    def call_api(self, prompt: str) -> Optional[str]:
        """Call the selected API provider"""
        if self.api_provider == "huggingface":
            return self.call_huggingface_api(prompt)
        elif self.api_provider == "gemini":
            return self.call_gemini_api(prompt)
        elif self.api_provider == "together":
            return self.call_together_api(prompt)
        elif self.api_provider == "openrouter":
            return self.call_openrouter_api(prompt)
        else:
            print(f"Unknown API provider: {self.api_provider}")
            return None
    
    def convert_division(self, chunk_file: str) -> Optional[str]:
        """Convert a single division chunk to Java (returns code only, no file save)"""
        print(f"Converting: {chunk_file}")
        
        with open(chunk_file, 'r') as f:
            division_data = json.load(f)
        
        division_type = division_data['metadata']['division_type']
        prompt = self.create_conversion_prompt(division_data, division_type)
        
        java_code = self.call_api(prompt)
        
        if java_code:
            # Clean up the response - extract Java code from markdown blocks
            cleaned_code = self.extract_java_code(java_code)
            print(f"✅ Converted {division_type.replace('_', ' ').title()}")
            return cleaned_code
        else:
            print(f"❌ Failed to convert {chunk_file}")
            return None
    
    def extract_java_code(self, response: str) -> str:
        """Extract Java code from LLM response, removing markdown formatting"""
        import re
        
        # Remove markdown code blocks
        code_blocks = re.findall(r'```java\n(.*?)\n```', response, re.DOTALL)
        if code_blocks:
            return '\n\n'.join(code_blocks)
        
        # If no code blocks, try to find Java-like content
        lines = response.split('\n')
        java_lines = []
        in_java_section = False
        
        for line in lines:
            # Skip explanatory text
            if any(phrase in line.lower() for phrase in ['here is', 'converted', 'equivalent', 'following']):
                continue
            
            # Detect Java code patterns
            if any(keyword in line for keyword in ['public class', 'import ', 'package ', '// ', '/*', '*/', '{', '}']):
                in_java_section = True
            
            if in_java_section and line.strip():
                java_lines.append(line)
        
        return '\n'.join(java_lines) if java_lines else response
    
    def convert_from_manifest(self, manifest_file: str) -> Dict[str, Any]:
        """Convert all chunks based on manifest file"""
        with open(manifest_file, 'r') as f:
            manifest = json.load(f)
        
        results = {
            "converted_divisions": [],
            "failed_conversions": [],
            "manifest": manifest,
            "api_provider": self.api_provider
        }
        
        chunks_dir = Path(manifest_file).parent
        conversion_order = manifest.get("conversion_order", [])
        
        print(f"Converting {len(conversion_order)} divisions using {self.api_provider}...")
        
        for division_type in conversion_order:
            if division_type in manifest["divisions"]:
                chunk_file = chunks_dir / manifest["divisions"][division_type]["chunk_file"]
                
                if chunk_file.exists():
                    java_code = self.convert_division(str(chunk_file))
                    if java_code:
                        results["converted_divisions"].append({
                            "division": division_type,
                            "chunk_file": str(chunk_file),
                            "java_code": java_code
                        })
                    else:
                        results["failed_conversions"].append(str(chunk_file))
                else:
                    print(f"⚠️  Chunk file not found: {chunk_file}")
                    results["failed_conversions"].append(str(chunk_file))
        
        # Create only the combined Java file
        combined_file = self.create_combined_java_file(results, manifest)
        results["combined_java_file"] = combined_file
        
        return results
    
    def create_combined_java_file(self, results: Dict[str, Any], manifest: Dict[str, Any]) -> str:
        """Combine all converted Java code into a single program"""
        base_name = manifest.get("base_name", "ConvertedProgram")
        combined_file = self.output_dir / f"{base_name}_Combined.java"
        
        with open(combined_file, 'w') as f:
            f.write(f"// Combined Java conversion from COBOL program\n")
            f.write(f"// Original source: {manifest.get('source_file', 'Unknown')}\n")
            f.write(f"// API Provider: {self.api_provider}\n\n")
            
            for conversion in results["converted_divisions"]:
                f.write(f"\n// ===== {conversion['division'].replace('_', ' ').title()} =====\n")
                f.write(conversion['java_code'])
                f.write('\n\n')
        
        print(f"📄 Created combined Java file: {combined_file}")
        return str(combined_file)

def main():
    import sys
    
    print("🆓 Free API LLM Converter")
    print("Available providers: huggingface, together, openrouter, gemini")
    print("Note: Some providers require API keys for better performance\n")
    
    if len(sys.argv) < 2:
        print("Usage: python free_api_converter.py <manifest_file> [provider]")
        print("Example: python free_api_converter.py output/chunks/FILE_IO_TEST_ast_manifest.json gemini")
        print("Note: Set environment variables as needed (e.g., GEMINI_API_KEY, TOGETHER_API_KEY)")
        sys.exit(1)
    
    manifest_file = sys.argv[1]
    provider = sys.argv[2] if len(sys.argv) > 2 else "huggingface"
    api_key = None  # Will be read from environment variable
    
    if not os.path.exists(manifest_file):
        print(f"Error: Manifest file not found: {manifest_file}")
        sys.exit(1)
    
    converter = FreeAPIConverter(provider, api_key)
    
    print(f"🚀 Using {provider} API...")
    if not api_key and provider != "huggingface":
        print("⚠️  No API key provided. Some providers may have limited functionality.")
    
    results = converter.convert_from_manifest(manifest_file)
    
    print(f"\n🎉 Conversion completed!")
    print(f"✅ Successfully converted: {len(results['converted_divisions'])} divisions")
    print(f"❌ Failed conversions: {len(results['failed_conversions'])}")
    print(f"📄 Combined Java file: {results.get('combined_java_file', 'Not created')}")
    print(f"📁 Output directory: {converter.output_dir}")

if __name__ == "__main__":
    main()
