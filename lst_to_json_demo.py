#!/usr/bin/env python3
"""
LST to JSON Demo - Show direct LST JSON conversion
"""

import sys
import json
from pathlib import Path

# Add paths
parsers_path = Path(__file__).parent / "engine" / "modernizers" / "cobol_system" / "parsers"
cobol85_path = parsers_path / "cobol85"
sys.path.insert(0, str(parsers_path))
sys.path.insert(0, str(cobol85_path))

from cobol_lst import parse_cobol_source, LosslessNode

def lst_node_to_dict(node: LosslessNode) -> dict:
    """Convert LST node to dictionary for JSON serialization."""
    return {
        "rule_name": node.rule_name,
        "text": node.text[:100] + "..." if len(node.text) > 100 else node.text,  # Truncate for readability
        "metadata": node.metadata,
        "children_count": len(node.children),
        "children": [lst_node_to_dict(child) for child in node.children[:5]]  # Limit children for demo
    }

def compare_lst_vs_ast():
    """Compare LST and AST output."""
    simple_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MESSAGE PIC X(20) VALUE 'HELLO WORLD'.
       
       PROCEDURE DIVISION.
       DISPLAY MESSAGE.
       STOP RUN.
    """
    
    print("="*60)
    print("LST (Lossless Syntax Tree) - Direct ANTLR4 Output")
    print("="*60)
    
    # Generate LST
    lst_root, tokens = parse_cobol_source(simple_cobol)
    lst_dict = lst_node_to_dict(lst_root)
    print(json.dumps(lst_dict, indent=2))
    
    print(f"\nLST Statistics:")
    print(f"- Root rule: {lst_root.rule_name}")
    print(f"- Total children: {len(lst_root.children)}")
    print(f"- Raw text length: {len(lst_root.text)} chars")
    print(f"- Token count: {len(tokens)}")
    
    print("\n" + "="*60)
    print("AST (Abstract Syntax Tree) - Cleaned & Enhanced")
    print("="*60)
    
    # Generate AST for comparison
    from cobol_ast_generator import generate_cobol_ast
    ast_root = generate_cobol_ast(simple_cobol)
    print(ast_root.to_json(indent=2))
    
    print(f"\nAST Statistics:")
    print(f"- Root type: {ast_root.node_type.value}")
    print(f"- Total children: {len(ast_root.children)}")
    print(f"- Enhanced attributes: {bool(ast_root.attributes)}")

def show_lst_detail():
    """Show detailed LST structure."""
    simple_cobol = "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO."
    
    lst_root, tokens = parse_cobol_source(simple_cobol)
    
    print("="*60)
    print("DETAILED LST STRUCTURE")
    print("="*60)
    
    def print_lst_tree(node, indent=0):
        prefix = "  " * indent
        print(f"{prefix}- {node.rule_name}")
        print(f"{prefix}  text: '{node.text.strip()}'")
        if node.metadata:
            print(f"{prefix}  metadata: {node.metadata}")
        
        for child in node.children[:3]:  # Limit for readability
            print_lst_tree(child, indent + 1)
        
        if len(node.children) > 3:
            print(f"{prefix}  ... and {len(node.children) - 3} more children")
    
    print_lst_tree(lst_root)
    
    print(f"\nAll ANTLR4 Tokens ({len(tokens)}):")
    for i, token in enumerate(tokens[:10]):  # Show first 10 tokens
        if hasattr(token, 'text') and hasattr(token, 'type'):
            print(f"  {i}: '{token.text}' (type: {token.type})")
    if len(tokens) > 10:
        print(f"  ... and {len(tokens) - 10} more tokens")

if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "detail":
        show_lst_detail()
    else:
        compare_lst_vs_ast()
