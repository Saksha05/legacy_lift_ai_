#!/usr/bin/env python3
"""
COBOL AST Parser - Main Interface

Simple interface to generate Abstract Syntax Trees from COBOL code.
"""

import sys
import os
from pathlib import Path

# Add the engine path to sys.path
engine_path = Path(__file__).parent / "engine" / "modernizers" / "cobol_system" / "parsers"
sys.path.insert(0, str(engine_path))

try:
    # Add the full path to the parsers directory
    parsers_path = Path(__file__).parent / "engine" / "modernizers" / "cobol_system" / "parsers"
    if str(parsers_path) not in sys.path:
        sys.path.insert(0, str(parsers_path))
    
    # Also add the cobol85 directory for ANTLR imports
    cobol85_path = parsers_path / "cobol85"
    if str(cobol85_path) not in sys.path:
        sys.path.insert(0, str(cobol85_path))
    
    from cobol_ast_generator import generate_cobol_ast, generate_cobol_ast_from_file
except ImportError as e:
    print(f"Error importing AST generator: {e}")
    print("Make sure the ANTLR4 dependencies are installed:")
    print("pip install antlr4-python3-runtime")
    sys.exit(1)

def parse_cobol_file_to_ast(file_path: str, output_format: str = "json", save_to_file: str = None):
    """
    Parse a COBOL file and generate its AST.
    
    Args:
        file_path: Path to the COBOL file
        output_format: Output format ('json' or 'dict')
        save_to_file: Optional path to save JSON output to file
    """
    try:
        print(f"Parsing COBOL file: {file_path}")
        
        # Generate AST with optional JSON saving
        ast_root = generate_cobol_ast_from_file(file_path, save_json=save_to_file)
        
        if not save_to_file:  # Only print if not saving to file
            if output_format.lower() == "json":
                print("\n" + "="*60)
                print("COBOL ABSTRACT SYNTAX TREE (JSON)")
                print("="*60)
                print(ast_root.to_json(indent=2))
            else:
                print("\n" + "="*60)
                print("COBOL ABSTRACT SYNTAX TREE (DICT)")
                print("="*60)
                print(ast_root.to_dict())
        
        return ast_root
        
    except Exception as e:
        print(f"Error parsing COBOL file: {e}")
        return None

def parse_cobol_source_to_ast(source_code: str, output_format: str = "json"):
    """
    Parse COBOL source code and generate its AST.
    
    Args:
        source_code: COBOL source code as string
        output_format: Output format ('json' or 'dict')
    """
    try:
        print("Parsing COBOL source code...")
        
        # Generate AST
        ast_root = generate_cobol_ast(source_code)
        
        if output_format.lower() == "json":
            print("\n" + "="*60)
            print("COBOL ABSTRACT SYNTAX TREE (JSON)")
            print("="*60)
            print(ast_root.to_json(indent=2))
        else:
            print("\n" + "="*60)
            print("COBOL ABSTRACT SYNTAX TREE (DICT)")
            print("="*60)
            print(ast_root.to_dict())
            
        return ast_root
        
    except Exception as e:
        print(f"Error parsing COBOL source: {e}")
        return None

def main():
    """Main function to demonstrate AST generation."""
    if len(sys.argv) < 2:
        print("Usage:")
        print("  python cobol_ast_parser.py <cobol_file>")
        print("  python cobol_ast_parser.py <cobol_file> [json|dict]")
        print("  python cobol_ast_parser.py <cobol_file> [json|dict] [output_file.json]")
        print("\nExamples:")
        print("  python cobol_ast_parser.py examples/cobol/CBL0001.cobol")
        print("  python cobol_ast_parser.py examples/cobol/CBL0001.cobol json output/CBL0001_ast.json")
        return
    
    file_path = sys.argv[1]
    output_format = "json" if len(sys.argv) < 3 else sys.argv[2].lower()
    output_file = sys.argv[3] if len(sys.argv) > 3 else None
    
    if not os.path.exists(file_path):
        print(f"Error: File '{file_path}' not found.")
        return
    
    # If output file is not specified, generate a default path in the output directory
    if output_file is None and output_format == "json":
        # Create output directory if it doesn't exist
        output_dir = Path("output")
        output_dir.mkdir(exist_ok=True)
        
        # Generate output filename based on input filename
        input_filename = Path(file_path).stem
        output_file = str(output_dir / f"{input_filename}_ast.json")
    
    # Parse the file
    ast_root = parse_cobol_file_to_ast(file_path, output_format, output_file)
    
    if ast_root:
        print(f"\n[SUCCESS] Generated AST for {file_path}")
        print(f"   - Root node type: {ast_root.node_type.value}")
        print(f"   - Total children: {len(ast_root.children)}")
        if output_file:
            print(f"   - JSON saved to: {output_file}")
    else:
        print(f"\n[ERROR] Failed to generate AST for {file_path}")

if __name__ == "__main__":
    main()
