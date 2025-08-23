#!/usr/bin/env python3
"""
AST to Java Converter

Converts COBOL AST JSON to Java code.
"""

import json
import sys
from pathlib import Path
from typing import Dict, List, Any

class CobolToJavaConverter:
    """Converts COBOL AST to Java code."""
    
    def __init__(self):
        self.java_code = []
        self.class_name = "CobolProgram"
        self.package_name = "com.legacy2modern.cobol"
        self.imports = set()
        self.fields = []
        self.methods = []
    
    def convert_ast_to_java(self, ast_dict: Dict[str, Any]) -> str:
        """Convert AST dictionary to Java code."""
        self.java_code = []
        self.imports = {"java.math.BigDecimal", "java.io.*", "java.util.*"}
        
        # Process the AST
        self._process_program_node(ast_dict)
        
        # Generate Java code
        return self._generate_java_class()
    
    def _process_program_node(self, node: Dict[str, Any]):
        """Process the root program node."""
        if node.get("type") == "program":
            for child in node.get("children", []):
                self._process_node(child)
    
    def _process_node(self, node: Dict[str, Any]):
        """Process any AST node."""
        node_type = node.get("type", "")
        
        if node_type == "identification_division":
            self._process_identification_division(node)
        elif node_type == "data_division":
            self._process_data_division(node)
        elif node_type == "procedure_division":
            self._process_procedure_division(node)
        elif node_type == "working_storage":
            self._process_working_storage(node)
        elif node_type == "data_item":
            self._process_data_item(node)
        elif node_type == "paragraph":
            self._process_paragraph(node)
        elif node_type == "statement":
            self._process_statement(node)
        
        # Process children
        for child in node.get("children", []):
            self._process_node(child)
    
    def _process_identification_division(self, node: Dict[str, Any]):
        """Process identification division."""
        attributes = node.get("attributes", {})
        program_id = attributes.get("program_id", "CobolProgram")
        author = attributes.get("author", "")
        
        # Clean up program ID (remove AUTHOR suffix if present)
        if "AUTHOR" in program_id:
            program_id = program_id.replace("AUTHOR", "").strip()
        
        self.class_name = self._to_java_class_name(program_id)
        
        # Add class documentation
        self.java_code.append("/**")
        self.java_code.append(f" * Generated from COBOL program: {program_id}")
        if author:
            self.java_code.append(f" * Original author: {author}")
        self.java_code.append(" * Converted by Legacy2Modern AST Converter")
        self.java_code.append(" */")
    
    def _process_data_division(self, node: Dict[str, Any]):
        """Process data division."""
        # Data division processing will be handled by child nodes
        pass
    
    def _process_working_storage(self, node: Dict[str, Any]):
        """Process working storage section."""
        # Working storage items will be converted to class fields
        pass
    
    def _process_data_item(self, node: Dict[str, Any]):
        """Process data item (variable declaration)."""
        name = node.get("name", "")
        attributes = node.get("attributes", {})
        
        java_type = attributes.get("java_type", "String")
        java_field = attributes.get("java_field_declaration", "")
        
        if java_field:
            self.fields.append(f"    {java_field}")
        elif name:
            java_name = self._to_java_field_name(name)
            self.fields.append(f"    private {java_type} {java_name};")
    
    def _process_procedure_division(self, node: Dict[str, Any]):
        """Process procedure division."""
        # Add main method
        if not any("main" in method for method in self.methods):
            self.methods.append("    public static void main(String[] args) {")
            self.methods.append(f"        {self.class_name} program = new {self.class_name}();")
            self.methods.append("        program.execute();")
            self.methods.append("    }")
            self.methods.append("")
            
            self.methods.append("    public void execute() {")
            self.methods.append("        // Main program execution")
    
    def _process_paragraph(self, node: Dict[str, Any]):
        """Process paragraph (convert to method)."""
        name = node.get("name", "").replace(".", "").strip()
        if name and name not in ["PROGRAM-ID", "AUTHOR"]:
            java_method_name = self._to_java_method_name(name)
            
            self.methods.append(f"    public void {java_method_name}() {{")
            self.methods.append(f"        // {name} paragraph implementation")
            
            # Process statements in paragraph
            for child in node.get("children", []):
                if child.get("type") == "statement":
                    stmt_text = child.get("attributes", {}).get("full_text", "")
                    java_equiv = child.get("attributes", {}).get("java_equivalent", "")
                    
                    if java_equiv and java_equiv != f"// {child.get('attributes', {}).get('statement_type', '')} statement conversion needed":
                        self.methods.append(f"        {java_equiv}")
                    else:
                        self.methods.append(f"        // TODO: Convert COBOL statement: {stmt_text}")
            
            self.methods.append("    }")
            self.methods.append("")
    
    def _process_statement(self, node: Dict[str, Any]):
        """Process individual statement."""
        attributes = node.get("attributes", {})
        stmt_type = attributes.get("statement_type", "")
        java_equiv = attributes.get("java_equivalent", "")
        full_text = attributes.get("full_text", "")
        
        if java_equiv and "conversion needed" not in java_equiv:
            self.methods.append(f"        {java_equiv}")
        else:
            self.methods.append(f"        // TODO: {stmt_type} - {full_text}")
    
    def _generate_java_class(self) -> str:
        """Generate the complete Java class."""
        lines = []
        
        # Package declaration
        lines.append(f"package {self.package_name};")
        lines.append("")
        
        # Imports
        for imp in sorted(self.imports):
            lines.append(f"import {imp};")
        lines.append("")
        
        # Class documentation and declaration
        lines.extend(self.java_code)
        lines.append(f"public class {self.class_name} {{")
        lines.append("")
        
        # Fields
        if self.fields:
            lines.append("    // Data fields from COBOL working storage")
            lines.extend(self.fields)
            lines.append("")
        
        # Constructor
        lines.append(f"    public {self.class_name}() {{")
        lines.append("        // Initialize fields")
        lines.append("    }}")
        lines.append("")
        
        # Methods
        if self.methods:
            lines.extend(self.methods)
        
        # Close execute method if it was opened
        if any("public void execute()" in method for method in self.methods):
            lines.append("    }")
        
        # Close class
        lines.append("}")
        
        return "\n".join(lines)
    
    def _to_java_class_name(self, cobol_name: str) -> str:
        """Convert COBOL program name to Java class name."""
        # Remove special characters and convert to PascalCase
        clean_name = ''.join(c for c in cobol_name if c.isalnum())
        if not clean_name:
            return "CobolProgram"
        
        return clean_name[0].upper() + clean_name[1:].lower()
    
    def _to_java_field_name(self, cobol_name: str) -> str:
        """Convert COBOL field name to Java field name."""
        parts = cobol_name.lower().replace('-', '_').split('_')
        if len(parts) == 1:
            return parts[0]
        
        return parts[0] + ''.join(word.capitalize() for word in parts[1:])
    
    def _to_java_method_name(self, cobol_name: str) -> str:
        """Convert COBOL paragraph name to Java method name."""
        parts = cobol_name.lower().replace('-', '_').split('_')
        if len(parts) == 1:
            return parts[0]
        
        return parts[0] + ''.join(word.capitalize() for word in parts[1:])

def convert_json_to_java(json_input: str) -> str:
    """Convert JSON AST to Java code."""
    try:
        # Parse JSON
        if json_input.strip().startswith('{'):
            ast_dict = json.loads(json_input)
        else:
            # Assume it's a file path
            with open(json_input, 'r', encoding='utf-8') as f:
                ast_dict = json.load(f)
        
        # Convert to Java
        converter = CobolToJavaConverter()
        java_code = converter.convert_ast_to_java(ast_dict)
        
        return java_code
        
    except Exception as e:
        return f"Error converting to Java: {e}"

def main():
    """Main function."""
    if len(sys.argv) < 2:
        print("Usage:")
        print("  python ast_to_java_converter.py <json_file>")
        print("  python ast_to_java_converter.py <json_string>")
        print("\nExample:")
        print("  python ast_to_java_converter.py CBL0001_ast.json")
        return
    
    json_input = sys.argv[1]
    java_code = convert_json_to_java(json_input)
    
    print("="*60)
    print("GENERATED JAVA CODE")
    print("="*60)
    print(java_code)
    
    # Save to file
    output_file = "GeneratedCobolProgram.java"
    if len(sys.argv) > 2:
        output_file = sys.argv[2]
    
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(java_code)
    
    print(f"\n[SUCCESS] Java code saved to: {output_file}")

if __name__ == "__main__":
    main()
