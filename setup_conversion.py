#!/usr/bin/env python3
"""
Setup script for COBOL to Java conversion system
"""

import os
import subprocess
import sys
from pathlib import Path

def check_ollama_installation():
    """Check if Ollama is installed and running"""
    try:
        result = subprocess.run(['ollama', 'list'], capture_output=True, text=True)
        return result.returncode == 0
    except FileNotFoundError:
        return False

def install_ollama_model(model_name="llama3.2"):
    """Install Llama model via Ollama"""
    try:
        print(f"Installing {model_name} model...")
        result = subprocess.run(['ollama', 'pull', model_name], check=True)
        return True
    except (subprocess.CalledProcessError, FileNotFoundError):
        return False

def create_directories():
    """Create necessary output directories"""
    dirs = [
        "output",
        "output/chunks", 
        "output/java_conversion"
    ]
    
    for dir_path in dirs:
        Path(dir_path).mkdir(parents=True, exist_ok=True)
        print(f"Created directory: {dir_path}")

def test_chunking(ast_file="FILE_IO_TEST_ast.json"):
    """Test the AST chunking functionality"""
    if not os.path.exists(ast_file):
        print(f"AST file not found: {ast_file}")
        return False
    
    try:
        from ast_chunker import ASTChunker
        chunker = ASTChunker(ast_file, "output/chunks")
        result = chunker.chunk_ast()
        return bool(result)
    except Exception as e:
        print(f"Chunking test failed: {e}")
        return False

def main():
    print("🔧 Setting up COBOL to Java conversion system...")
    
    # Create directories
    print("\n📁 Creating output directories...")
    create_directories()
    
    # Test chunking
    print("\n🔪 Testing AST chunking...")
    if test_chunking():
        print("✅ Chunking test passed")
    else:
        print("❌ Chunking test failed")
    
    # Check Ollama
    print("\n🤖 Checking Ollama installation...")
    if check_ollama_installation():
        print("✅ Ollama is installed and running")
        
        # Try to install model
        if install_ollama_model():
            print("✅ Llama model installed")
        else:
            print("⚠️  Could not install Llama model automatically")
            print("   Run manually: ollama pull llama3.2")
    else:
        print("❌ Ollama not found")
        print("   Install from: https://ollama.ai/")
        print("   Then run: ollama serve")
        print("   And: ollama pull llama3.2")
    
    print("\n📋 Usage Examples:")
    print("1. Chunk existing AST:")
    print("   python ast_chunker.py FILE_IO_TEST_ast.json")
    
    print("\n2. Convert with Llama (requires Ollama running):")
    print("   python llama_converter.py output/chunks/FILE_IO_TEST_manifest.json")
    
    print("\n3. Full pipeline:")
    print("   python cobol_to_java_pipeline.py examples/cobol/HELLO.cobol")
    
    print("\n4. Batch convert all COBOL files:")
    print("   python cobol_to_java_pipeline.py --batch examples/cobol/")

if __name__ == "__main__":
    main()
