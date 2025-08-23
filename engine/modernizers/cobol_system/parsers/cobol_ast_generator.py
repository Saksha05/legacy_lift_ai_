"""
COBOL Abstract Syntax Tree Generator

This module generates clean Abstract Syntax Trees from COBOL code using the existing ANTLR parser.
"""

from typing import List, Dict, Any, Optional, Union
from dataclasses import dataclass, field
from enum import Enum
import json
try:
    from .cobol_lst import parse_cobol_source, parse_cobol_file, LosslessNode
except ImportError:
    from cobol_lst import parse_cobol_source, parse_cobol_file, LosslessNode

class ASTNodeType(Enum):
    """AST node types for COBOL structures."""
    PROGRAM = "program"
    IDENTIFICATION_DIVISION = "identification_division"
    ENVIRONMENT_DIVISION = "environment_division"
    DATA_DIVISION = "data_division"
    PROCEDURE_DIVISION = "procedure_division"
    SECTION = "section"
    PARAGRAPH = "paragraph"
    STATEMENT = "statement"
    DATA_ITEM = "data_item"
    FILE_DESCRIPTION = "file_description"
    WORKING_STORAGE = "working_storage"
    PROCEDURE_CALL = "procedure_call"
    CONDITION = "condition"
    EXPRESSION = "expression"
    LITERAL = "literal"
    IDENTIFIER = "identifier"

@dataclass
class ASTNode:
    """Clean Abstract Syntax Tree node."""
    node_type: ASTNodeType
    name: str = ""
    value: Any = None
    children: List['ASTNode'] = field(default_factory=list)
    attributes: Dict[str, Any] = field(default_factory=dict)
    
    def add_child(self, child: 'ASTNode'):
        """Add a child node."""
        self.children.append(child)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert AST to dictionary representation."""
        return {
            "type": self.node_type.value,
            "name": self.name,
            "value": self.value,
            "attributes": self.attributes,
            "children": [child.to_dict() for child in self.children]
        }
    
    def to_json(self, indent: int = 2) -> str:
        """Convert AST to JSON string."""
        return json.dumps(self.to_dict(), indent=indent)
    
    def save_to_json(self, file_path: str, indent: int = 2) -> None:
        """Save AST to JSON file."""
        with open(file_path, 'w', encoding='utf-8') as f:
            json.dump(self.to_dict(), f, indent=indent)
        print(f"AST saved to: {file_path}")

class CobolASTGenerator:
    """Generates clean AST from COBOL LST."""
    
    def __init__(self):
        self.ast_root: Optional[ASTNode] = None
        # Simple symbol table mapping identifier name -> DATA_ITEM ASTNode
        # Scope handling can be expanded later; for now, WORKING-STORAGE items
        # and FILE SECTION items populate this table.
        self.symbol_table: Dict[str, ASTNode] = {}
    
    def generate_ast(self, lst_root: LosslessNode) -> ASTNode:
        """Generate AST from LST root node."""
        self.ast_root = ASTNode(ASTNodeType.PROGRAM, name="COBOL_PROGRAM")
        self._process_node(lst_root, self.ast_root)
        return self.ast_root
    
    def _process_node(self, lst_node: LosslessNode, parent_ast: ASTNode):
        """Process LST node and convert to AST."""
        rule_name = lst_node.rule_name.lower()
        
        # Map LST rules to AST nodes
        if "identification" in rule_name and "division" in rule_name:
            ast_node = ASTNode(ASTNodeType.IDENTIFICATION_DIVISION, name="IDENTIFICATION DIVISION")
            parent_ast.add_child(ast_node)
            self._extract_program_info(lst_node, ast_node)
            # Avoid double-processing children for specialized handlers
            return
            
        elif "environment" in rule_name and "division" in rule_name:
            ast_node = ASTNode(ASTNodeType.ENVIRONMENT_DIVISION, name="ENVIRONMENT DIVISION")
            parent_ast.add_child(ast_node)
            self._extract_environment_info(lst_node, ast_node)
            return
            
        elif "data" in rule_name and "division" in rule_name:
            ast_node = ASTNode(ASTNodeType.DATA_DIVISION, name="DATA DIVISION")
            parent_ast.add_child(ast_node)
            self._extract_data_info(lst_node, ast_node)
            return
            
        elif "procedure" in rule_name and "division" in rule_name:
            ast_node = ASTNode(ASTNodeType.PROCEDURE_DIVISION, name="PROCEDURE DIVISION")
            parent_ast.add_child(ast_node)
            self._extract_procedure_info(lst_node, ast_node)
            return
            
        elif "section" in rule_name:
            # Sections are created by specialized extractors; skip generic creation to reduce duplicates
            ast_node = parent_ast
            
        elif "paragraph" in rule_name:
            # Paragraphs are processed in _extract_procedure_info; skip generic creation
            ast_node = parent_ast
            
        elif "statement" in rule_name:
            stmt_text = lst_node.get_text().strip()
            ast_node = ASTNode(ASTNodeType.STATEMENT, name=stmt_text[:50])
            ast_node.attributes["full_text"] = stmt_text
            parent_ast.add_child(ast_node)
            
        elif "datadescription" in rule_name:
            self._process_data_description(lst_node, parent_ast)
            return
        
        else:
            # For other nodes, continue processing children
            ast_node = parent_ast
        
        # Process children
        for child in lst_node.children:
            self._process_node(child, ast_node if 'ast_node' in locals() else parent_ast)
    
    def _extract_program_info(self, lst_node: LosslessNode, ast_node: ASTNode):
        """Extract program identification information."""
        text = lst_node.get_text()
        
        # Extract PROGRAM-ID
        if "PROGRAM-ID" in text:
            lines = text.split('\n')
            for line in lines:
                if "PROGRAM-ID" in line:
                    parts = line.split()
                    if len(parts) >= 2:
                        program_id = parts[1].rstrip('.')
                        ast_node.attributes["program_id"] = program_id
                        break
        
        # Extract AUTHOR
        if "AUTHOR" in text:
            lines = text.split('\n')
            for line in lines:
                if "AUTHOR" in line:
                    author = line.split("AUTHOR.")[1].strip() if "AUTHOR." in line else ""
                    ast_node.attributes["author"] = author
                    break
    
    def _extract_environment_info(self, lst_node: LosslessNode, ast_node: ASTNode):
        """Extract environment division information."""
        text = lst_node.get_text()
        
        # Extract file control information
        if "SELECT" in text:
            selects = []
            lines = text.split('\n')
            for line in lines:
                if "SELECT" in line and "ASSIGN" in line:
                    select_info = {
                        "file_name": "",
                        "assign_to": ""
                    }
                    
                    # Extract file name and assignment
                    parts = line.strip().split()
                    select_idx = -1
                    assign_idx = -1
                    
                    for i, part in enumerate(parts):
                        if part == "SELECT":
                            select_idx = i
                        elif part == "ASSIGN":
                            assign_idx = i
                    
                    if select_idx >= 0 and select_idx + 1 < len(parts):
                        select_info["file_name"] = parts[select_idx + 1]
                    
                    if assign_idx >= 0 and assign_idx + 2 < len(parts):
                        select_info["assign_to"] = parts[assign_idx + 2].rstrip('.')
                    
                    selects.append(select_info)
            
            ast_node.attributes["file_control"] = selects
    
    def _extract_data_info(self, lst_node: LosslessNode, ast_node: ASTNode):
        # Look for file section
        file_section_nodes = lst_node.find_nodes_by_rule("FileSectionContext")
        print(f"Found {len(file_section_nodes)} FileSection nodes")
        for fs_node in file_section_nodes:
            fs_ast = ASTNode(ASTNodeType.SECTION, name="FILE SECTION")
            ast_node.add_child(fs_ast)
            self._process_file_descriptions(fs_node, fs_ast)
        
        # Look for working storage section
        ws_section_nodes = lst_node.find_nodes_by_rule("WorkingStorageSectionContext")
        print(f"Found {len(ws_section_nodes)} WorkingStorageSection nodes")
        for ws_node in ws_section_nodes:
            print("Processing Working-Storage section")
            ws_ast = ASTNode(ASTNodeType.WORKING_STORAGE, name="WORKING-STORAGE SECTION")
            ast_node.add_child(ws_ast)
            self._process_data_descriptions(ws_node, ws_ast)
            print(f"Working-Storage section now has {len(ws_ast.children)} children")
    
    def _process_file_descriptions(self, lst_node: LosslessNode, parent_ast: ASTNode):
        """Process file descriptions."""
        fd_nodes = lst_node.find_nodes_by_rule("FileDescriptionEntryContext")
        for fd_node in fd_nodes:
            fd_text = fd_node.get_text()
            fd_name = self._extract_fd_name(fd_text)
            
            fd_ast = ASTNode(ASTNodeType.FILE_DESCRIPTION, name=fd_name)
            fd_ast.attributes["description"] = fd_text.strip()
            parent_ast.add_child(fd_ast)
    
    def _parse_pic_clause(self, pic_clause: str) -> dict:
        """Parse a PIC clause and return type information."""
        result = {
            "type": "unknown",
            "length": 0,
            "decimal_places": 0,
            "signed": False,
            "usage": "DISPLAY"  # Default usage
        }
        
        # Simple PIC clause parsing - this can be enhanced as needed
        if 'X' in pic_clause or 'x' in pic_clause:
            result["type"] = "alphanumeric"
            # Extract length if specified in parentheses
            if '(' in pic_clause and ')' in pic_clause:
                length_str = pic_clause.split('(')[1].split(')')[0]
                result["length"] = int(length_str) if length_str.isdigit() else 1
            else:
                result["length"] = len([c for c in pic_clause if c.upper() == 'X'])
        elif '9' in pic_clause:
            if 'V' in pic_clause or 'v' in pic_clause:
                result["type"] = "decimal"
                parts = pic_clause.upper().split('V')
                result["length"] = len(parts[0].replace('9', '').replace('S', '').replace(',', '').replace('.', ''))
                result["decimal_places"] = len(parts[1].replace('9', '').replace(',', '').replace('.', ''))
            else:
                result["type"] = "numeric"
                result["length"] = len(pic_clause.replace('9', '').replace('S', '').replace(',', '').replace('.', ''))
            
            result["signed"] = 'S' in pic_clause or 's' in pic_clause
        
        return result

    def _process_data_descriptions(self, section_node: LosslessNode, ast_node: ASTNode):
        """Process data description entries in a section."""
        data_entries = section_node.find_nodes_by_rule("DataDescriptionEntryContext")
        print(f"Found {len(data_entries)} data description entries in section")
        for i, entry in enumerate(data_entries, 1):
            print(f"Processing data description entry {i}/{len(data_entries)}")
            self._process_data_description(entry, ast_node)
            print(f"AST node now has {len(ast_node.children)} children after processing entry {i}")
    
    def _get_child_text(self, node: LosslessNode, rule_name: str) -> str:
        """Helper to get text of a child node by rule name."""
        for child in node.children:
            if hasattr(child, 'rule_name') and child.rule_name == rule_name:
                return child.get_text().strip()
        return ""

    def _extract_data_item_details(self, text: str) -> dict:
        """Extract data item details from the raw text."""
        details = {
            'name': 'FILLER',
            'level': '01',
            'usage': 'DISPLAY',
            'pic': None,
            'value': None,
            'is_filler': True
        }
        
        # Split into tokens while preserving quoted strings
        tokens = []
        in_quotes = False
        current_token = ''
        
        for char in text:
            if char.isspace() and not in_quotes:
                if current_token:
                    tokens.append(current_token)
                    current_token = ''
                continue
                
            if char in ("'", '"'):
                in_quotes = not in_quotes
                
            current_token += char
            
        if current_token:
            tokens.append(current_token)
        
        # Process tokens to extract details
        i = 0
        while i < len(tokens):
            token = tokens[i].upper()
            
            # Level number
            if token.isdigit() and len(token) <= 2 and i == 0:
                details['level'] = token
                i += 1
                continue
                
            # Data name
            if i == 1 and token != 'FILLER' and not token.startswith(('PIC', 'PICTURE', 'USAGE', 'VALUE')):
                details['name'] = token
                details['is_filler'] = False
                i += 1
                continue
                
            # PIC/PICTURE clause
            if token in ('PIC', 'PICTURE'):
                if i + 1 < len(tokens):
                    details['pic'] = tokens[i+1]
                    i += 2
                    continue
                    
            # USAGE clause
            if token == 'USAGE' or (token.startswith('USAGE-') and 'IS' in token):
                if 'IS' in token:
                    usage = token.split('IS', 1)[1].strip()
                elif i + 1 < len(tokens):
                    usage = tokens[i+1]
                    if usage == 'IS' and i + 2 < len(tokens):
                        usage = tokens[i+2]
                        i += 1
                details['usage'] = usage
                i += 2
                continue
                
            # VALUE clause
            if token == 'VALUE' and i + 1 < len(tokens):
                details['value'] = tokens[i+1].strip("'\"")
                i += 2
                continue
                
            i += 1
            
        return details
        
    def _extract_data_item_details(self, text: str) -> dict:
        """Extract data item details from the raw text."""
        details = {
            'name': 'FILLER',
            'level': '01',
            'usage': 'DISPLAY',
            'pic': None,
            'value': None,
            'is_filler': True
        }
        
        # Split into lines and process each line
        lines = [line.strip() for line in text.split('\n') if line.strip()]
        if not lines:
            return details
            
        # Process the first line for level, name, and basic attributes
        first_line = lines[0]
        
        # Extract level number (first 2-3 digits)
        level = ''
        for i, c in enumerate(first_line):
            if c.isdigit():
                level += c
            else:
                break
        if level:
            details['level'] = level
            first_line = first_line[len(level):].lstrip()
        
        # Extract name (up to the next space or PIC/VALUE)
        name_end = len(first_line)
        for marker in ['PIC', 'PICTURE', 'VALUE', ' ']:
            idx = first_line.find(marker)
            if 0 < idx < name_end:
                name_end = idx
        
        if name_end > 0:
            details['name'] = first_line[:name_end].strip()
            details['is_filler'] = details['name'].upper() == 'FILLER'
            first_line = first_line[name_end:].lstrip()
        
        # Process PIC/PICTURE clause
        pic_markers = ['PIC ', 'PICTURE ']
        for marker in pic_markers:
            if marker in first_line.upper():
                pic_start = first_line.upper().index(marker) + len(marker)
                pic_end = len(first_line)
                # Look for the end of the PIC clause
                for term in ['VALUE', 'USAGE', ' ']:
                    idx = first_line.upper().find(term, pic_start)
                    if idx > pic_start and idx < pic_end:
                        pic_end = idx
                details['pic'] = first_line[pic_start:pic_end].strip()
                first_line = first_line[pic_end:].lstrip()
                break
        
        # Process VALUE clause
        if 'VALUE' in first_line.upper():
            value_start = first_line.upper().index('VALUE') + 5
            value_str = first_line[value_start:].strip()
            # Handle quoted values
            if value_str and value_str[0] in ('"', "'"):
                quote = value_str[0]
                end_quote = value_str[1:].find(quote) + 1
                if end_quote > 0:
                    details['value'] = quote + value_str[1:end_quote+1]
            else:
                # Unquoted value (e.g., numeric)
                value_end = len(value_str)
                for term in [' ', '.']:
                    idx = value_str.find(term)
                    if 0 < idx < value_end:
                        value_end = idx
                details['value'] = value_str[:value_end].strip()
        
        return details
        
    def _process_data_description(self, node: LosslessNode, parent_ast: ASTNode) -> ASTNode:
        """Process a single data description entry."""
        # Get the full text of the data description
        text = node.get_text().strip()
        if not text:
            return None
            
        # Extract details from the text
        details = self._extract_data_item_details(text)
        
        # Create a data item node with all details
        data_item = ASTNode(ASTNodeType.DATA_ITEM, name=details['name'])
        data_item.attributes.update({
            'level': details['level'],
            'usage': details['usage'],
            'is_filler': details['is_filler']
        })
        
        # Add PIC clause if present
        if details['pic']:
            data_item.attributes['pic'] = details['pic']
            
        # Add VALUE clause if present
        if details['value'] is not None:
            data_item.attributes['value'] = details['value']
            
        parent_ast.add_child(data_item)
        
        return data_item
            
        # Get PIC clause if it exists
        pic_clause = self._get_child_text(node, "dataPictureClause")
        if pic_clause:
            # Clean up the PIC clause
            pic_clause = ' '.join(pic_clause.split())  # Normalize whitespace
            print(f"  Found PIC clause: {pic_clause}")
            
            # Store the original PIC clause
            data_item.attributes["pic"] = pic_clause
            
            # Parse PIC clause type and length
            if "PIC" in pic_clause or "PICTURE" in pic_clause:
                # Extract the actual format (e.g., 'X(80)' from 'PIC X(80)')
                format_part = pic_clause.split(maxsplit=1)[1] if ' ' in pic_clause else ''
                pic_info = self._parse_pic_clause(format_part)
                
                # Update attributes with PIC clause details
                data_item.attributes.update({
                    "data_type": pic_info["type"],
                    "length": pic_info["length"],
                    "decimal_places": pic_info["decimal_places"],
                    "signed": pic_info["signed"],
                    "usage": pic_info["usage"] or data_item.attributes.get("usage", "DISPLAY")
                })
        
        # Get VALUE clause if it exists
        value_clause = self._get_child_text(node, "dataValueClause")
        if value_clause:
            # Clean up the VALUE clause
            value_clause = ' '.join(value_clause.split())  # Normalize whitespace
            print(f"  Found VALUE clause: {value_clause}")
            
            # Extract just the value part (after VALUE keyword)
            if 'VALUE' in value_clause:
                value = value_clause.split('VALUE', 1)[1].strip()
                # Remove any trailing period
                if value.endswith('.'):
                    value = value[:-1].strip()
                data_item.attributes["value"] = value
                
                # For numeric values, store as appropriate type
                if 'data_type' in data_item.attributes:
                    if data_item.attributes['data_type'] in ['numeric', 'decimal']:
                        try:
                            # Remove any currency symbols and thousands separators
                            clean_value = value.replace('$', '').replace(',', '')
                            if '.' in clean_value:
                                data_item.attributes["numeric_value"] = float(clean_value)
                            else:
                                data_item.attributes["numeric_value"] = int(clean_value)
                        except (ValueError, TypeError):
                            pass  # Keep as string if conversion fails
        
        # Check for REDEFINES clause
        redefines_clause = self._get_child_text(node, "dataRedefinesClause")
        if redefines_clause:
            redefines_clause = ' '.join(redefines_clause.split())  # Normalize whitespace
            print(f"  Found REDEFINES clause: {redefines_clause}")
            if 'REDEFINES' in redefines_clause:
                redefines = redefines_clause.split('REDEFINES', 1)[1].strip()
                data_item.attributes["redefines"] = redefines
        
        # Check for OCCURS clause
        occurs_clause = self._get_child_text(node, "dataOccursClause")
        if occurs_clause:
            occurs_clause = ' '.join(occurs_clause.split())  # Normalize whitespace
            print(f"  Found OCCURS clause: {occurs_clause}")
            data_item.attributes["occurs"] = occurs_clause
            
            # Try to extract the number of occurrences
            if 'OCCURS' in occurs_clause:
                occurs_parts = occurs_clause.split()
                if len(occurs_parts) > 1 and occurs_parts[0] == 'OCCURS':
                    try:
                        times = int(occurs_parts[1])
                        data_item.attributes["occurs_times"] = times
                    except (ValueError, IndexError):
                        pass
            
        return data_item
        
        level = None
        name = None
        pic = None
        value = None
        
        # Extract level number
        if tokens[0].isdigit():
            level = tokens[0]
        
        # Extract variable name
        if len(tokens) > 1 and not tokens[1].startswith('PIC'):
            name = tokens[1].rstrip('.')
        
        # Extract PIC clause
        for i, token in enumerate(tokens):
            if token == 'PIC' and i + 1 < len(tokens):
                pic = tokens[i + 1].rstrip('.')
                break
        
        # Extract VALUE clause
        for i, token in enumerate(tokens):
            if token == 'VALUE' and i + 1 < len(tokens):
                value = tokens[i + 1].rstrip('.')
                break
        
        if name and level:
            data_ast = ASTNode(ASTNodeType.DATA_ITEM, name=name)
            
            # Enhanced attributes with PIC parsing and Java type mapping
            parsed_pic = self._parse_pic(pic) if pic else {}
            java_type = self._get_java_type_from_pic(pic) if pic else "String"
            data_ast.attributes = {
                "level": level,
                "pic": pic,
                "pic_parsed": parsed_pic,
                "value": value,
                "full_text": text,
                "java_type": java_type,
                "java_field_declaration": self._generate_java_field(name, java_type, value),
                "cobol_semantics": self._get_cobol_semantics(pic, level)
            }
            parent_ast.add_child(data_ast)
            # Register in symbol table for reference linking
            self.symbol_table[name.upper()] = data_ast
    
    def _extract_procedure_info(self, lst_node: LosslessNode, ast_node: ASTNode):
        """Extract procedure division information."""
        # Process paragraphs
        para_nodes = lst_node.find_nodes_by_rule("ParagraphContext")
        for para_node in para_nodes:
            para_name = self._extract_paragraph_name(para_node)
            if para_name:
                para_ast = ASTNode(ASTNodeType.PARAGRAPH, name=para_name)
                ast_node.add_child(para_ast)
                
                # Process statements in paragraph
                self._process_statements(para_node, para_ast)
    
    def _process_statements(self, lst_node: LosslessNode, parent_ast: ASTNode):
        """Process statements within a paragraph."""
        stmt_nodes = lst_node.find_nodes_by_rule("StatementContext")
        for stmt_node in stmt_nodes:
            stmt_text = stmt_node.get_text().strip()
            if stmt_text:
                stmt_type = self._classify_statement(stmt_text)
                stmt_ast = ASTNode(ASTNodeType.STATEMENT, name=stmt_text[:50])
                # Parse referenced identifiers for linking
                referenced_ids = self._find_identifiers(stmt_text)
                # Attach condition node if applicable
                condition_node: Optional[ASTNode] = None
                if stmt_type == 'IF':
                    cond = self._parse_condition(stmt_text)
                    condition_node = ASTNode(ASTNodeType.CONDITION, name="condition", attributes=cond)
                    stmt_ast.add_child(condition_node)
                elif stmt_type == 'PERFORM' and 'UNTIL' in stmt_text.upper():
                    cond = self._parse_condition(stmt_text, keyword='UNTIL')
                    condition_node = ASTNode(ASTNodeType.CONDITION, name="until_condition", attributes=cond)
                    stmt_ast.add_child(condition_node)
                stmt_ast.attributes = {
                    "full_text": stmt_text,
                    "statement_type": stmt_type,
                    "java_equivalent": self._get_java_equivalent(stmt_text, stmt_type),
                    "cobol_semantics": self._get_statement_semantics(stmt_text, stmt_type),
                    "referenced_identifiers": referenced_ids
                }
                parent_ast.add_child(stmt_ast)
    
    def _classify_statement(self, stmt_text: str) -> str:
        """Classify the type of COBOL statement."""
        stmt_upper = stmt_text.upper().strip()
        
        if stmt_upper.startswith('MOVE'):
            return 'MOVE'
        elif stmt_upper.startswith('PERFORM'):
            return 'PERFORM'
        elif stmt_upper.startswith('READ'):
            return 'READ'
        elif stmt_upper.startswith('WRITE'):
            return 'WRITE'
        elif stmt_upper.startswith('OPEN'):
            return 'OPEN'
        elif stmt_upper.startswith('CLOSE'):
            return 'CLOSE'
        elif stmt_upper.startswith('IF'):
            return 'IF'
        elif stmt_upper.startswith('GOBACK'):
            return 'GOBACK'
        else:
            return 'OTHER'
    
    def _get_java_type_from_pic(self, pic: str) -> str:
        """Convert COBOL PIC clause to Java type."""
        if not pic:
            return 'String'
        
        pic = pic.upper()
        
        if '9' in pic and 'V' in pic:
            return 'BigDecimal'
        elif '9' in pic and 'S' in pic:
            return 'long'
        elif '9' in pic:
            return 'int'
        elif 'X' in pic:
            return 'String'
        elif 'A' in pic:
            return 'String'
        else:
            return 'String'

    def _parse_pic(self, pic: Optional[str]) -> Dict[str, Any]:
        """Parse PIC clause into structured details: kind, length, scale, signed, usage."""
        if not pic:
            return {}
        p = pic.upper()
        # Normalize like X(80), 9(5)V9(2), S9(4) COMP-3, etc.
        result: Dict[str, Any] = {
            "raw": pic,
            "kind": None,
            "length": None,
            "scale": 0,
            "signed": 'S' in p,
            "usage": None
        }
        # Usage detection
        for usage in ["COMP-3", "COMP", "BINARY", "DISPLAY"]:
            if usage in p:
                result["usage"] = usage
                break
        # Identify kind and lengths
        import re
        # Extract numeric with optional decimal
        if '9' in p:
            result["kind"] = "numeric"
            # Remove non pattern suffixes (e.g., COMP-3)
            core = p.split()[0]
            # Count digits pattern 9( n ) possibly multiple segments
            def _count(segment: str) -> int:
                m = re.match(r"^(S?9)(\((\d+)\))?$", segment)
                if not m:
                    return segment.count('9')
                return int(m.group(3) or 1)
            total = 0
            scale = 0
            if 'V' in core:
                left, right = core.split('V', 1)
                total = sum(_count(seg) for seg in re.findall(r"S?9(?:\(\d+\))?", left)) + \
                        sum(_count(seg) for seg in re.findall(r"9(?:\(\d+\))?", right))
                scale = sum(_count(seg) for seg in re.findall(r"9(?:\(\d+\))?", right))
            else:
                total = sum(_count(seg) for seg in re.findall(r"S?9(?:\(\d+\))?", core))
            result["length"] = total
            result["scale"] = scale
        elif 'X' in p or 'A' in p:
            result["kind"] = "alphanumeric"
            import re
            core = p
            # Count X(n) or A(n)
            m = re.search(r"[XA]\((\d+)\)", core)
            if m:
                result["length"] = int(m.group(1))
            else:
                # Count sequence length if no parens
                result["length"] = p.count('X') + p.count('A')
        else:
            result["kind"] = "unknown"
        return result
    
    def _generate_java_field(self, name: str, java_type: str, value: str = None) -> str:
        """Generate Java field declaration."""
        java_name = self._cobol_to_java_name(name)
        
        if value and value != 'SPACE' and value != 'SPACES':
            if java_type == 'String':
                clean_value = value.strip("'\"")
                return f'private {java_type} {java_name} = "{clean_value}";'
            else:
                return f'private {java_type} {java_name} = {value};'
        else:
            return f'private {java_type} {java_name};'
    
    def _cobol_to_java_name(self, cobol_name: str) -> str:
        """Convert COBOL variable name to Java camelCase."""
        parts = cobol_name.lower().split('-')
        if len(parts) == 1:
            return parts[0]
        
        return parts[0] + ''.join(word.capitalize() for word in parts[1:])
    
    def _get_cobol_semantics(self, pic: str, level: str) -> dict:
        """Get COBOL semantic information for LLM context."""
        semantics = {
            "is_group_item": level and level in ['01', '77'] and not pic,
            "is_elementary": bool(pic),
            "storage_type": "working_storage"
        }
        
        if pic:
            pic_upper = pic.upper()
            semantics.update({
                "is_numeric": '9' in pic_upper,
                "is_signed": 'S' in pic_upper,
                "has_decimal": 'V' in pic_upper,
                "is_display": 'X' in pic_upper or 'A' in pic_upper,
                "is_packed": 'COMP-3' in pic_upper
            })
        
        return semantics
    
    def _get_java_equivalent(self, stmt_text: str, stmt_type: str) -> str:
        """Generate Java equivalent for COBOL statement."""
        stmt_upper = stmt_text.upper().strip()
        
        if stmt_type == 'MOVE':
            # Parse MOVE statement: MOVE source TO target
            parts = stmt_upper.split()
            if 'TO' in parts:
                to_idx = parts.index('TO')
                if to_idx > 1 and to_idx + 1 < len(parts):
                    source = ' '.join(parts[1:to_idx])
                    target = ' '.join(parts[to_idx + 1:])
                    java_target = self._cobol_to_java_name(target.rstrip('.'))
                    java_source = self._cobol_to_java_name(source) if source.replace('-', '').isalpha() else source
                    return f'{java_target} = {java_source};'
            return '// MOVE statement conversion needed'
        
        elif stmt_type == 'PERFORM':
            if 'UNTIL' in stmt_upper:
                return 'while (condition) { /* loop body */ }'
            else:
                # Simple PERFORM - method call
                parts = stmt_upper.split()
                if len(parts) > 1:
                    method_name = self._cobol_to_java_name(parts[1].rstrip('.'))
                    return f'{method_name}();'
            return '// PERFORM statement conversion needed'
            
        elif stmt_type == 'READ':
            return '// File I/O - use BufferedReader or similar'
            
        elif stmt_type == 'WRITE':
            return '// File I/O - use PrintWriter or similar'
            
        elif stmt_type == 'OPEN':
            return '// Initialize file streams'
            
        elif stmt_type == 'CLOSE':
            return '// Close file streams'
            
        elif stmt_type == 'IF':
            return 'if (condition) { /* statements */ }'
            
        elif stmt_type == 'GOBACK':
            return 'return;'
            
        else:
            return f'// {stmt_type} statement conversion needed'

    def _find_identifiers(self, stmt_text: str) -> List[Dict[str, Any]]:
        """Find identifiers referenced in a statement and link to declarations if known."""
        upper_to_decl = self.symbol_table
        # Tokenize on non-word and keep hyphens for COBOL names
        import re
        tokens = re.findall(r"[A-Z0-9-]+|'[^']*'|\S", stmt_text.upper())
        refs: List[Dict[str, Any]] = []
        for tok in tokens:
            if not tok or tok.startswith("'"):
                continue
            if tok in {"MOVE","TO","IF","NOT","=" ,"READ","WRITE","OPEN","CLOSE","PERFORM","UNTIL","END-IF","THEN","ELSE","GIVING","ADD","SUBTRACT","FROM","MULTIPLY","BY","DIVIDE","INTO","DISPLAY","ACCEPT"}:
                continue
            decl = upper_to_decl.get(tok)
            if decl:
                refs.append({
                    "name": tok,
                    "decl_ref": {
                        "path": ["DATA_DIVISION"],
                        "java_type": decl.attributes.get("java_type"),
                        "level": decl.attributes.get("level")
                    }
                })
        return refs

    def _parse_condition(self, stmt_text: str, keyword: str = 'IF') -> Dict[str, Any]:
        """Parse a simple condition like IF A = B, IF A NOT = 'Y', or UNTIL EOF = 'Y'."""
        import re
        text = stmt_text.strip()
        U = text.upper()
        if keyword == 'IF':
            # Extract after IF up to THEN/END-IF/period or first statement separator
            m = re.search(r"\bIF\b(.*)$", U)
        else:
            m = re.search(r"\bUNTIL\b(.*)$", U)
        cond_raw = m.group(1).strip() if m else U
        # Stop at first keyword that likely begins body
        cond_raw = re.split(r"\bTHEN\b|\bPERFORM\b|\bDISPLAY\b|\bMOVE\b|\bREAD\b|\.|\bEND-IF\b", cond_raw, 1)[0].strip()
        # Handle NOT = pattern
        negated = False
        cond_norm = cond_raw
        cond_norm = cond_norm.replace(" NOT =", " !=").replace(" =", " = ").replace(" !=", " != ")
        # Split by operators
        if " != " in cond_norm:
            op = "!="
            left, right = cond_norm.split(" != ", 1)
        elif " = " in cond_norm:
            op = "=="
            left, right = cond_norm.split(" = ", 1)
        else:
            # Fallback: return raw
            return {
                "raw": cond_raw
            }
        left = left.strip()
        right = right.strip()
        # Remove trailing tokens after right operand if any
        right = re.split(r"\s+(AND|OR|THEN|END-IF|\.)\b", right)[0].strip()
        return {
            "operator": op,
            "left": left,
            "right": right,
            "raw": cond_raw
        }
    
    def _get_statement_semantics(self, stmt_text: str, stmt_type: str) -> dict:
        """Get semantic information about COBOL statement."""
        return {
            "operation_type": stmt_type,
            "is_data_movement": stmt_type == 'MOVE',
            "is_control_flow": stmt_type in ['PERFORM', 'IF', 'GOBACK'],
            "is_io_operation": stmt_type in ['READ', 'WRITE', 'OPEN', 'CLOSE'],
            "requires_loop_conversion": 'UNTIL' in stmt_text.upper(),
            "requires_method_call": stmt_type == 'PERFORM' and 'UNTIL' not in stmt_text.upper()
        }
    
    def _extract_name(self, lst_node: LosslessNode) -> str:
        """Extract name from LST node."""
        text = lst_node.get_text().strip()
        if text:
            return text.split()[0] if text.split() else text
        return ""
    
    def _extract_fd_name(self, fd_text: str) -> str:
        """Extract file description name."""
        tokens = fd_text.strip().split()
        if len(tokens) >= 2 and tokens[0] == 'FD':
            return tokens[1]
        return ""
    
    def _extract_paragraph_name(self, para_node: LosslessNode) -> str:
        """Extract paragraph name."""
        text = para_node.get_text().strip()
        lines = text.split('\n')
        for line in lines:
            line = line.strip()
            if line and not line.startswith('*') and '.' in line:
                # First non-comment line with a period is likely the paragraph name
                name = line.split('.')[0].strip()
                if name and not any(keyword in name.upper() for keyword in ['MOVE', 'PERFORM', 'READ', 'WRITE', 'OPEN', 'CLOSE']):
                    return name
        return ""

def generate_cobol_ast(cobol_source: str) -> ASTNode:
    """
    Generate AST from COBOL source code.
    
    Args:
        cobol_source: COBOL source code as string
        
    Returns:
        AST root node
    """
    # Parse using existing LST parser
    lst_root, tokens = parse_cobol_source(cobol_source)
    
    # Generate AST from LST
    generator = CobolASTGenerator()
    ast_root = generator.generate_ast(lst_root)
    
    return ast_root

def generate_cobol_ast_from_file(file_path: str, save_json: str = None) -> ASTNode:
    """
    Generate AST from COBOL file.
    
    Args:
        file_path: Path to COBOL file
        save_json: Optional path to save JSON output
        
    Returns:
        AST root node
    """
    # Parse using existing LST parser
    lst_root, tokens = parse_cobol_file(file_path)
    
    # Generate AST from LST
    generator = CobolASTGenerator()
    ast_root = generator.generate_ast(lst_root)
    
    # Save to JSON if requested
    if save_json:
        ast_root.save_to_json(save_json)
    
    return ast_root

def generate_cobol_ast(cobol_source: str, save_json: str = None) -> ASTNode:
    """
    Generate AST from COBOL source code.
    
    Args:
        cobol_source: COBOL source code as string
        save_json: Optional path to save JSON output
        
    Returns:
        AST root node
    """
    # Parse using existing LST parser
    lst_root, tokens = parse_cobol_source(cobol_source)
    
    # Generate AST from LST
    generator = CobolASTGenerator()
    ast_root = generator.generate_ast(lst_root)
    
    # Save to JSON if requested
    if save_json:
        ast_root.save_to_json(save_json)
    
    return ast_root
