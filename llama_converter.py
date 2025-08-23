#!/usr/bin/env python3
"""
Llama Converter - Converts COBOL AST chunks to Java using local Llama model
"""

import json
import os
import requests
from pathlib import Path
from typing import Dict, List, Any, Optional

class LlamaConverter:
    def __init__(self, llama_url: str = "http://localhost:11434", model_name: str = "llama3.2"):
        self.llama_url = llama_url
        self.model_name = model_name
        self.output_dir = Path("output/java_conversion")
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
    def check_llama_connection(self) -> bool:
        """Check if Llama is running locally"""
        try:
            response = requests.get(f"{self.llama_url}/api/tags", timeout=5)
            return response.status_code == 200
        except requests.exceptions.RequestException:
            return False
    
    def create_conversion_prompt(self, division_data: Dict[str, Any], division_type: str) -> str:
        """Create a specialized prompt for each division type"""
        
        base_prompt = f"""You are a COBOL to Java conversion expert. Convert the following COBOL {division_type.replace('_', ' ').title()} AST to equivalent Java code.

COBOL Division Type: {division_type}
AST Data: {json.dumps(division_data['ast_node'], indent=2)}

Conversion Guidelines:
"""
        
        if division_type == "identification_division":
            prompt = base_prompt + """
- Convert to Java package declaration and class definition
- Extract program name for class name
- Add appropriate imports
- Create class-level documentation from COBOL comments
"""
        
        elif division_type == "environment_division":
            prompt = base_prompt + """
- Convert file control entries to Java file handling imports
- Map COBOL file assignments to Java File/Path objects
- Create configuration constants for file paths
- Add necessary Java I/O imports
"""
        
        elif division_type == "data_division":
            prompt = base_prompt + """
- Convert COBOL data items to Java class fields
- Map COBOL PIC clauses to appropriate Java data types
- Convert COBOL level numbers to nested class structures
- Handle COBOL OCCURS clauses as Java arrays/collections
- Convert COBOL REDEFINES to Java unions or separate fields
"""
        
        elif division_type == "procedure_division":
            prompt = base_prompt + """
- Convert COBOL paragraphs to Java methods
- Map COBOL PERFORM statements to method calls or loops
- Convert COBOL IF-ELSE to Java conditional statements
- Handle COBOL file I/O with Java streams
- Convert COBOL arithmetic to Java operations
- Map COBOL MOVE statements to Java assignments
"""
        
        prompt += """

Requirements:
1. Generate clean, readable Java code
2. Add comments explaining COBOL-specific conversions
3. Use modern Java practices (Java 8+)
4. Handle data type conversions appropriately
5. Maintain the original program logic
6. Add error handling where appropriate

Output only the Java code with minimal explanation."""
        
        return prompt
    
    def call_llama(self, prompt: str) -> Optional[str]:
        """Call local Llama model with the conversion prompt"""
        try:
            payload = {
                "model": self.model_name,
                "prompt": prompt,
                "stream": False,
                "options": {
                    "temperature": 0.1,
                    "top_p": 0.9,
                    "num_predict": 2000
                }
            }
            
            response = requests.post(
                f"{self.llama_url}/api/generate",
                json=payload,
                timeout=120
            )
            
            if response.status_code == 200:
                result = response.json()
                return result.get("response", "")
            else:
                print(f"Llama API error: {response.status_code}")
                print(f"Response: {response.text}")
                print(f"URL: {self.llama_url}/api/generate")
                print(f"Model: {self.model_name}")
                return None
                
        except requests.exceptions.RequestException as e:
            print(f"Error calling Llama: {e}")
            return None
    
    def convert_division(self, chunk_file: str) -> Optional[str]:
        """Convert a single division chunk to Java"""
        print(f"Converting: {chunk_file}")
        
        with open(chunk_file, 'r') as f:
            division_data = json.load(f)
        
        division_type = division_data['metadata']['division_type']
        prompt = self.create_conversion_prompt(division_data, division_type)
        
        java_code = self.call_llama(prompt)
        
        if java_code:
            # Save the Java conversion
            base_name = Path(chunk_file).stem
            java_file = self.output_dir / f"{base_name}.java"
            
            with open(java_file, 'w') as f:
                f.write(f"// Converted from COBOL {division_type.replace('_', ' ').title()}\n")
                f.write(f"// Source: {chunk_file}\n\n")
                f.write(java_code)
            
            print(f"✅ Saved Java code to: {java_file}")
            return str(java_file)
        else:
            print(f"❌ Failed to convert {chunk_file}")
            return None
    
    def convert_from_manifest(self, manifest_file: str) -> Dict[str, Any]:
        """Convert all chunks based on manifest file"""
        with open(manifest_file, 'r') as f:
            manifest = json.load(f)
        
        results = {
            "converted_files": [],
            "failed_conversions": [],
            "manifest": manifest
        }
        
        chunks_dir = Path(manifest_file).parent
        conversion_order = manifest.get("conversion_order", [])
        
        print(f"Converting {len(conversion_order)} divisions in order...")
        
        for division_type in conversion_order:
            if division_type in manifest["divisions"]:
                chunk_file = chunks_dir / manifest["divisions"][division_type]["chunk_file"]
                
                if chunk_file.exists():
                    java_file = self.convert_division(str(chunk_file))
                    if java_file:
                        results["converted_files"].append({
                            "division": division_type,
                            "chunk_file": str(chunk_file),
                            "java_file": java_file
                        })
                    else:
                        results["failed_conversions"].append(str(chunk_file))
                else:
                    print(f"⚠️  Chunk file not found: {chunk_file}")
                    results["failed_conversions"].append(str(chunk_file))
        
        # Create a combined Java file
        self.create_combined_java_file(results, manifest)
        
        return results
    
    def create_combined_java_file(self, results: Dict[str, Any], manifest: Dict[str, Any]):
        """Combine all converted Java files into a single program"""
        base_name = manifest.get("base_name", "ConvertedProgram")
        combined_file = self.output_dir / f"{base_name}_Combined.java"
        
        with open(combined_file, 'w') as f:
            f.write(f"// Combined Java conversion from COBOL program\n")
            f.write(f"// Original source: {manifest.get('source_file', 'Unknown')}\n\n")
            
            for conversion in results["converted_files"]:
                f.write(f"\n// ===== {conversion['division'].replace('_', ' ').title()} =====\n")
                
                if os.path.exists(conversion['java_file']):
                    with open(conversion['java_file'], 'r') as java_f:
                        content = java_f.read()
                        # Skip the header comments we added
                        lines = content.split('\n')
                        start_idx = 0
                        for i, line in enumerate(lines):
                            if not line.strip().startswith('//'):
                                start_idx = i
                                break
                        f.write('\n'.join(lines[start_idx:]))
                        f.write('\n')
        
        print(f"📄 Created combined Java file: {combined_file}")

def main():
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python llama_converter.py <manifest_file> [llama_url] [model_name]")
        print("Example: python llama_converter.py output/chunks/FILE_IO_TEST_manifest.json")
        sys.exit(1)
    
    manifest_file = sys.argv[1]
    llama_url = sys.argv[2] if len(sys.argv) > 2 else "http://localhost:11434"
    model_name = sys.argv[3] if len(sys.argv) > 3 else "llama3.2"
    
    if not os.path.exists(manifest_file):
        print(f"Error: Manifest file not found: {manifest_file}")
        sys.exit(1)
    
    converter = LlamaConverter(llama_url, model_name)
    
    print("Checking Llama connection...")
    if not converter.check_llama_connection():
        print(f"❌ Cannot connect to Llama at {llama_url}")
        print("Make sure Ollama is running: ollama serve")
        print(f"And the model is available: ollama pull {model_name}")
        sys.exit(1)
    
    print(f"✅ Connected to Llama at {llama_url}")
    
    results = converter.convert_from_manifest(manifest_file)
    
    print(f"\n🎉 Conversion completed!")
    print(f"✅ Successfully converted: {len(results['converted_files'])} divisions")
    print(f"❌ Failed conversions: {len(results['failed_conversions'])}")
    print(f"📁 Java files saved to: {converter.output_dir}")

if __name__ == "__main__":
    main()
