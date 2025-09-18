#!/usr/bin/env python3
"""
COBOL to Java Converter using Gemini API

Converts COBOL code directly to Java using Google Gemini API.
"""

import os
import sys
import json
import requests
from pathlib import Path
from typing import Optional
import urllib3

# Suppress SSL warnings
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

class CobolToJavaConverter:
    """Converts COBOL code directly to Java using Gemini API."""
    
    def __init__(self, api_key: Optional[str] = None):
        self.api_key = api_key or os.getenv("GEMINI_API_KEY")
        if not self.api_key:
            raise ValueError("GEMINI_API_KEY environment variable is required")
        
        self.api_url = "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent"
        self.headers = {"Content-Type": "application/json"}
    
    def convert_cobol_to_java(self, cobol_code: str, program_name: str = "CobolProgram") -> str:
        """Convert COBOL code directly to Java using Gemini API."""
        
        prompt = f"""Convert this COBOL program to Java. Be concise and fast.

COBOL:
{cobol_code}

Generate a complete Java class named {program_name} with:
- Proper imports
- Main method
- Convert COBOL logic to Java
- Handle input/output operations

Return only Java code, no explanations."""

        try:
            response = self._call_gemini_api(prompt)
            if response:
                # Clean up the response to ensure it's pure Java code
                java_code = self._clean_java_response(response)
                return java_code
            else:
                return self._generate_fallback_java(program_name)
                
        except Exception as e:
            print(f"Error during conversion: {e}")
            return self._generate_fallback_java(program_name)
    
    def _call_gemini_api(self, prompt: str) -> Optional[str]:
        """Call Google Gemini API"""
        url = f"{self.api_url}?key={self.api_key}"
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
                headers=self.headers,
                json=payload,
                timeout=10,
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
            print(f"Gemini API call failed: {e}")
            return None
    
    def _clean_java_response(self, response: str) -> str:
        """Clean up Gemini's response to extract pure Java code."""
        # Remove markdown code blocks if present
        if "```java" in response:
            start = response.find("```java") + 7
            end = response.find("```", start)
            if end != -1:
                response = response[start:end].strip()
        elif "```" in response:
            start = response.find("```") + 3
            end = response.find("```", start)
            if end != -1:
                response = response[start:end].strip()
        
        # Remove any leading/trailing whitespace
        response = response.strip()
        
        # Ensure the response starts with package or import or class
        lines = response.split('\n')
        java_start = 0
        for i, line in enumerate(lines):
            line = line.strip()
            if (line.startswith('package ') or 
                line.startswith('import ') or 
                line.startswith('public class ') or
                line.startswith('class ')):
                java_start = i
                break
        
        return '\n'.join(lines[java_start:])
    
    def _generate_fallback_java(self, program_name: str) -> str:
        """Generate a basic Java template if API call fails."""
        class_name = self._to_java_class_name(program_name)
        
        return f"""package com.legacy2modern.cobol;

import java.util.Scanner;
import java.math.BigDecimal;

/**
 * Generated from COBOL program: {program_name}
 * Converted by Legacy2Modern Direct Converter
 * Note: This is a fallback template - manual conversion may be needed
 */
public class {class_name} {{
    
    private Scanner scanner = new Scanner(System.in);
    
    public {class_name}() {{
        // Initialize fields
    }}
    
    public static void main(String[] args) {{
        {class_name} program = new {class_name}();
        program.execute();
    }}
    
    public void execute() {{
        System.out.println("COBOL program converted to Java");
        System.out.println("Manual implementation required");
        // TODO: Implement COBOL business logic
    }}
}}"""
    
    def _to_java_class_name(self, cobol_name: str) -> str:
        """Convert COBOL program name to Java class name."""
        # Remove special characters and convert to PascalCase
        clean_name = ''.join(c for c in cobol_name if c.isalnum())
        if not clean_name:
            return "CobolProgram"
        
        return clean_name[0].upper() + clean_name[1:].lower()

def convert_cobol_to_java(cobol_input: str, program_name: str = "CobolProgram") -> str:
    """Convert COBOL code to Java code."""
    try:
        # Check if input is a file path or raw COBOL code
        if cobol_input.strip().endswith('.cobol') or cobol_input.strip().endswith('.cbl'):
            # It's a file path
            with open(cobol_input, 'r', encoding='utf-8') as f:
                cobol_code = f.read()
            # Extract program name from file
            program_name = Path(cobol_input).stem
        else:
            # It's raw COBOL code
            cobol_code = cobol_input
        
        # Convert using Gemini API
        converter = CobolToJavaConverter()
        java_code = converter.convert_cobol_to_java(cobol_code, program_name)
        
        return java_code
        
    except Exception as e:
        return f"Error converting COBOL to Java: {e}"

# Legacy compatibility function
def convert_json_to_java(json_input: str) -> str:
    """Legacy function for backward compatibility - now converts COBOL directly."""
    try:
        # If it's a JSON file, try to extract COBOL code from it
        if json_input.strip().startswith('{'):
            ast_dict = json.loads(json_input)
        else:
            # Assume it's a file path
            with open(json_input, 'r', encoding='utf-8') as f:
                ast_dict = json.load(f)
        
        # Try to extract original COBOL code from AST if available
        cobol_code = ast_dict.get('original_cobol', '')
        if not cobol_code:
            # If no original COBOL, create a basic template
            program_name = ast_dict.get('program_name', 'CobolProgram')
            converter = CobolToJavaConverter()
            return converter._generate_fallback_java(program_name)
        
        # Convert the extracted COBOL code
        return convert_cobol_to_java(cobol_code)
        
    except Exception as e:
        return f"Error converting JSON to Java: {e}"

def main():
    """Main function."""
    if len(sys.argv) < 2:
        print("Usage:")
        print("  python ast_to_java_converter.py <cobol_file>")
        print("  python ast_to_java_converter.py <cobol_code>")
        print("\nExample:")
        print("  python ast_to_java_converter.py examples/cobol/HELLO.cobol")
        print("  python ast_to_java_converter.py \"IDENTIFICATION DIVISION...\"")
        print("\nEnvironment:")
        print("  GEMINI_API_KEY must be set for API access")
        return
    
    cobol_input = sys.argv[1]
    
    # Check if GEMINI_API_KEY is set
    if not os.getenv("GEMINI_API_KEY"):
        print("Error: GEMINI_API_KEY environment variable is required")
        print("Please set your Gemini API key: export GEMINI_API_KEY=your_api_key")
        return
    
    java_code = convert_cobol_to_java(cobol_input)
    
    print("="*60)
    print("GENERATED JAVA CODE")
    print("="*60)
    print(java_code)
    
    # Save to file
    output_file = "GeneratedCobolProgram.java"
    if len(sys.argv) > 2:
        output_file = sys.argv[2]
    else:
        # Try to derive output filename from input
        if cobol_input.endswith('.cobol') or cobol_input.endswith('.cbl'):
            base_name = Path(cobol_input).stem
            output_file = f"{base_name}.java"
    
    try:
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(java_code)
        print(f"\n[SUCCESS] Java code saved to: {output_file}")
    except Exception as e:
        print(f"\n[ERROR] Failed to save Java code: {e}")

if __name__ == "__main__":
    main()
