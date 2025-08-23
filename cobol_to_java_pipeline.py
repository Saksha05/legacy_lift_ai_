#!/usr/bin/env python3
"""
COBOL to Java Pipeline - Complete conversion pipeline from AST to Java
"""

import os
import sys
import subprocess
from pathlib import Path
from ast_chunker import ASTChunker
from llama_converter import LlamaConverter

class CobolToJavaPipeline:
    def __init__(self, llama_url: str = "http://localhost:11434", model_name: str = "llama3.2"):
        self.llama_url = llama_url
        self.model_name = model_name
        
    def run_pipeline(self, cobol_file: str, output_base_dir: str = "output") -> Dict[str, Any]:
        """Run the complete COBOL to Java conversion pipeline"""
        
        print(f"🚀 Starting COBOL to Java conversion pipeline")
        print(f"📄 Input COBOL file: {cobol_file}")
        
        # Step 1: Generate AST from COBOL
        print("\n📊 Step 1: Generating AST from COBOL...")
        ast_file = self.generate_ast(cobol_file, output_base_dir)
        if not ast_file:
            return {"error": "Failed to generate AST"}
        
        # Step 2: Chunk AST by divisions
        print("\n🔪 Step 2: Chunking AST by divisions...")
        chunk_result = self.chunk_ast(ast_file, output_base_dir)
        if not chunk_result:
            return {"error": "Failed to chunk AST"}
        
        # Step 3: Convert chunks to Java using Llama
        print("\n🤖 Step 3: Converting to Java using Llama...")
        conversion_result = self.convert_to_java(chunk_result["manifest_file"])
        if not conversion_result:
            return {"error": "Failed to convert to Java"}
        
        return {
            "success": True,
            "cobol_file": cobol_file,
            "ast_file": ast_file,
            "chunks": chunk_result,
            "java_conversion": conversion_result,
            "output_directory": output_base_dir
        }
    
    def generate_ast(self, cobol_file: str, output_dir: str) -> str:
        """Generate AST from COBOL file"""
        if not os.path.exists(cobol_file):
            print(f"❌ COBOL file not found: {cobol_file}")
            return None
        
        base_name = Path(cobol_file).stem
        ast_file = f"{output_dir}/{base_name}_ast.json"
        
        try:
            cmd = [
                "python", "cobol_ast_parser.py",
                cobol_file, "json", ast_file
            ]
            
            result = subprocess.run(cmd, capture_output=True, text=True, check=True)
            print(f"✅ AST generated: {ast_file}")
            return ast_file
            
        except subprocess.CalledProcessError as e:
            print(f"❌ Failed to generate AST: {e}")
            print(f"Error output: {e.stderr}")
            return None
    
    def chunk_ast(self, ast_file: str, output_dir: str) -> Dict[str, Any]:
        """Chunk AST by divisions"""
        chunks_dir = f"{output_dir}/chunks"
        
        try:
            chunker = ASTChunker(ast_file, chunks_dir)
            result = chunker.chunk_ast()
            
            if result:
                print(f"✅ AST chunked into {len(result['chunk_files'])} divisions")
                return result
            else:
                print("❌ Failed to chunk AST")
                return None
                
        except Exception as e:
            print(f"❌ Error chunking AST: {e}")
            return None
    
    def convert_to_java(self, manifest_file: str) -> Dict[str, Any]:
        """Convert chunks to Java using Llama"""
        try:
            converter = LlamaConverter(self.llama_url, self.model_name)
            
            # Check Llama connection
            if not converter.check_llama_connection():
                print(f"❌ Cannot connect to Llama at {self.llama_url}")
                return None
            
            result = converter.convert_from_manifest(manifest_file)
            
            if result["converted_files"]:
                print(f"✅ Converted {len(result['converted_files'])} divisions to Java")
                return result
            else:
                print("❌ No divisions were successfully converted")
                return None
                
        except Exception as e:
            print(f"❌ Error converting to Java: {e}")
            return None
    
    def batch_convert(self, cobol_files: List[str], output_base_dir: str = "output") -> Dict[str, Any]:
        """Convert multiple COBOL files"""
        results = {
            "successful_conversions": [],
            "failed_conversions": [],
            "total_files": len(cobol_files)
        }
        
        for i, cobol_file in enumerate(cobol_files, 1):
            print(f"\n{'='*60}")
            print(f"Processing file {i}/{len(cobol_files)}: {Path(cobol_file).name}")
            print(f"{'='*60}")
            
            result = self.run_pipeline(cobol_file, output_base_dir)
            
            if result.get("success"):
                results["successful_conversions"].append(result)
            else:
                results["failed_conversions"].append({
                    "file": cobol_file,
                    "error": result.get("error", "Unknown error")
                })
        
        return results

def main():
    if len(sys.argv) < 2:
        print("Usage: python cobol_to_java_pipeline.py <cobol_file> [output_dir] [llama_url] [model_name]")
        print("   or: python cobol_to_java_pipeline.py --batch <cobol_directory> [output_dir] [llama_url] [model_name]")
        print("\nExamples:")
        print("  python cobol_to_java_pipeline.py examples/cobol/HELLO.cobol")
        print("  python cobol_to_java_pipeline.py --batch examples/cobol/ output")
        sys.exit(1)
    
    # Parse arguments
    if sys.argv[1] == "--batch":
        if len(sys.argv) < 3:
            print("Error: --batch requires a directory path")
            sys.exit(1)
        
        cobol_dir = sys.argv[2]
        output_dir = sys.argv[3] if len(sys.argv) > 3 else "output"
        llama_url = sys.argv[4] if len(sys.argv) > 4 else "http://localhost:11434"
        model_name = sys.argv[5] if len(sys.argv) > 5 else "llama3.2"
        
        # Find all COBOL files
        cobol_files = list(Path(cobol_dir).glob("*.cobol"))
        if not cobol_files:
            print(f"No .cobol files found in {cobol_dir}")
            sys.exit(1)
        
        pipeline = CobolToJavaPipeline(llama_url, model_name)
        results = pipeline.batch_convert([str(f) for f in cobol_files], output_dir)
        
        print(f"\n🎉 Batch conversion completed!")
        print(f"✅ Successful: {len(results['successful_conversions'])}")
        print(f"❌ Failed: {len(results['failed_conversions'])}")
        
    else:
        cobol_file = sys.argv[1]
        output_dir = sys.argv[2] if len(sys.argv) > 2 else "output"
        llama_url = sys.argv[3] if len(sys.argv) > 3 else "http://localhost:11434"
        model_name = sys.argv[4] if len(sys.argv) > 4 else "llama3.2"
        
        pipeline = CobolToJavaPipeline(llama_url, model_name)
        result = pipeline.run_pipeline(cobol_file, output_dir)
        
        if result.get("success"):
            print(f"\n🎉 Conversion completed successfully!")
            print(f"📁 Output directory: {result['output_directory']}")
        else:
            print(f"\n❌ Conversion failed: {result.get('error')}")
            sys.exit(1)

if __name__ == "__main__":
    main()
