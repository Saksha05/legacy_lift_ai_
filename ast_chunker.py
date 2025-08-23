#!/usr/bin/env python3
"""
AST Chunker - Splits COBOL AST JSON by divisions for Java conversion
"""

import json
import os
from pathlib import Path
from typing import Dict, List, Any

class ASTChunker:
    def __init__(self, ast_file_path: str, output_dir: str = "output/chunks"):
        self.ast_file_path = ast_file_path
        # Extract program name from AST file to create program-specific folder
        base_name = Path(ast_file_path).stem.replace("_ast", "")
        self.program_name = base_name
        self.output_dir = Path(output_dir) / base_name
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
    def load_ast(self) -> Dict[str, Any]:
        """Load the AST JSON file"""
        with open(self.ast_file_path, 'r') as f:
            return json.load(f)
    
    def extract_divisions(self, ast: Dict[str, Any]) -> Dict[str, Dict[str, Any]]:
        """Extract each division from the AST"""
        divisions = {}
        
        if ast.get("type") == "program" and "children" in ast:
            for child in ast["children"]:
                division_type = child.get("type", "")
                if division_type.endswith("_division"):
                    division_name = division_type.replace("_", " ").title()
                    divisions[division_type] = {
                        "division_name": division_name,
                        "ast_node": child,
                        "metadata": {
                            "original_file": self.ast_file_path,
                            "division_type": division_type,
                            "has_children": len(child.get("children", [])) > 0,
                            "attributes_count": len(child.get("attributes", {}))
                        }
                    }
        
        return divisions
    
    def save_division_chunks(self, divisions: Dict[str, Dict[str, Any]]) -> List[str]:
        """Save each division as a separate JSON file in program-specific folder"""
        saved_files = []
        
        for division_type, division_data in divisions.items():
            chunk_filename = f"{self.program_name}_{division_type}.json"
            chunk_path = self.output_dir / chunk_filename
            
            with open(chunk_path, 'w') as f:
                json.dump(division_data, f, indent=2)
            
            saved_files.append(str(chunk_path))
            print(f"Saved {division_data['division_name']} to: {chunk_path}")
        
        return saved_files
    
    def create_conversion_manifest(self, divisions: Dict[str, Dict[str, Any]]) -> str:
        """Create a manifest file for the conversion process in program-specific folder"""
        manifest_path = self.output_dir / f"{self.program_name}_manifest.json"
        
        manifest = {
            "source_file": self.ast_file_path,
            "program_name": self.program_name,
            "base_name": self.program_name,
            "output_folder": str(self.output_dir),
            "divisions": {},
            "conversion_order": ["identification_division", "environment_division", "data_division", "procedure_division"],
            "java_conversion_hints": {
                "identification_division": "Convert to class metadata and package declaration",
                "environment_division": "Convert to import statements and configuration",
                "data_division": "Convert to class fields and data structures",
                "procedure_division": "Convert to methods and main logic"
            }
        }
        
        for division_type, division_data in divisions.items():
            manifest["divisions"][division_type] = {
                "chunk_file": f"{self.program_name}_{division_type}.json",
                "chunk_path": str(self.output_dir / f"{self.program_name}_{division_type}.json"),
                "complexity": "high" if division_data["metadata"]["has_children"] else "low",
                "priority": 1 if division_type == "procedure_division" else 2
            }
        
        with open(manifest_path, 'w') as f:
            json.dump(manifest, f, indent=2)
        
        print(f"Created conversion manifest: {manifest_path}")
        return str(manifest_path)
    
    def chunk_ast(self) -> Dict[str, Any]:
        """Main method to chunk the AST"""
        print(f"Loading AST from: {self.ast_file_path}")
        print(f"Creating program-specific folder: {self.output_dir}")
        ast = self.load_ast()
        
        print("Extracting divisions...")
        divisions = self.extract_divisions(ast)
        
        if not divisions:
            print("No divisions found in AST!")
            return {}
        
        print(f"Found {len(divisions)} divisions for program '{self.program_name}':")
        for div_type, div_data in divisions.items():
            print(f"  - {div_data['division_name']} ({div_type})")
        
        print(f"\nSaving division chunks to: {self.output_dir}")
        saved_files = self.save_division_chunks(divisions)
        
        print("\nCreating conversion manifest...")
        manifest_file = self.create_conversion_manifest(divisions)
        
        return {
            "program_name": self.program_name,
            "divisions": divisions,
            "chunk_files": saved_files,
            "manifest_file": manifest_file,
            "output_directory": str(self.output_dir)
        }

def main():
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python ast_chunker.py <ast_file_path> [output_dir]")
        sys.exit(1)
    
    ast_file = sys.argv[1]
    output_dir = sys.argv[2] if len(sys.argv) > 2 else "output/chunks"
    
    if not os.path.exists(ast_file):
        print(f"Error: AST file not found: {ast_file}")
        sys.exit(1)
    
    chunker = ASTChunker(ast_file, output_dir)
    result = chunker.chunk_ast()
    
    if result:
        print(f"\n✅ Successfully chunked AST into {len(result['chunk_files'])} files")
        print(f"📁 Output directory: {result['output_directory']}")
        print(f"📋 Manifest file: {result['manifest_file']}")
    else:
        print("❌ Failed to chunk AST")
        sys.exit(1)

if __name__ == "__main__":
    main()
