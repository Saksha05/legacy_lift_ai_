#!/usr/bin/env python3
"""
Legacy Lift AI - Web Application
COBOL to Java Conversion Service
"""

from flask import Flask, request, jsonify, render_template, send_from_directory
from werkzeug.utils import secure_filename
import os
import json
import tempfile
import shutil
from pathlib import Path
import subprocess
import sys

# Import existing conversion modules
from ast_chunker import ASTChunker
from free_api_converter import FreeAPIConverter
from cobol_ast_parser import parse_cobol_file_to_ast

app = Flask(__name__)
app.config['MAX_CONTENT_LENGTH'] = 16 * 1024 * 1024  # 16MB max file size
app.config['UPLOAD_FOLDER'] = 'uploads'
app.config['OUTPUT_FOLDER'] = 'output'

# Ensure directories exist
os.makedirs(app.config['UPLOAD_FOLDER'], exist_ok=True)
os.makedirs(app.config['OUTPUT_FOLDER'], exist_ok=True)
os.makedirs('static/css', exist_ok=True)
os.makedirs('static/js', exist_ok=True)
os.makedirs('templates', exist_ok=True)

ALLOWED_EXTENSIONS = {'cobol', 'cob', 'cbl', 'txt'}

def allowed_file(filename):
    return '.' in filename and filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS

class COBOLProcessor:
    def __init__(self):
        self.temp_dir = None
        
    def process_cobol_file(self, file_path, api_provider='groq'):
        """Process COBOL file through the complete pipeline"""
        try:
            # Step 1: Parse COBOL to AST first
            base_name = Path(file_path).stem
            ast_output_file = f"output/{base_name}_ast.json"
            
            # Generate AST and save to file
            parse_cobol_file_to_ast(file_path, output_format="json", save_to_file=ast_output_file)
            
            if not os.path.exists(ast_output_file):
                return {'error': 'Failed to parse COBOL file to AST'}
            
            # Step 2: Create chunks from AST
            chunker = ASTChunker(ast_output_file)
            result = chunker.chunk_ast()
            
            if not result or 'manifest_file' not in result:
                return {'error': 'Failed to parse COBOL file'}
            
            manifest_file = result['manifest_file']
            
            # Step 3: Convert chunks to Java using LLM
            converter = FreeAPIConverter(api_provider=api_provider)
            conversion_result = converter.convert_from_manifest(manifest_file)
            
            if not conversion_result or 'combined_java_file' not in conversion_result:
                return {'error': 'Failed to convert to Java'}
            
            # Step 4: Read the generated Java code
            java_file = conversion_result['combined_java_file']
            with open(java_file, 'r', encoding='utf-8') as f:
                java_code = f.read()
            
            return {
                'success': True,
                'java_code': java_code,
                'ast_data': result.get('ast_data', {}),
                'chunks': conversion_result.get('converted_divisions', []),
                'stats': {
                    'divisions_converted': len(conversion_result.get('converted_divisions', [])),
                    'failed_conversions': len(conversion_result.get('failed_conversions', [])),
                    'api_provider': api_provider
                }
            }
            
        except Exception as e:
            return {'error': f'Processing failed: {str(e)}'}

processor = COBOLProcessor()

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/api/upload', methods=['POST'])
def upload_file():
    """Handle COBOL file upload and processing"""
    try:
        if 'file' not in request.files:
            return jsonify({'error': 'No file provided'}), 400
        
        file = request.files['file']
        if file.filename == '':
            return jsonify({'error': 'No file selected'}), 400
        
        if not allowed_file(file.filename):
            return jsonify({'error': 'Invalid file type. Please upload a COBOL file (.cobol, .cob, .cbl, .txt)'}), 400
        
        # Save uploaded file
        filename = secure_filename(file.filename)
        file_path = os.path.join(app.config['UPLOAD_FOLDER'], filename)
        file.save(file_path)
        
        # Get API provider from request
        api_provider = request.form.get('api_provider', 'groq')
        
        # Process the file
        result = processor.process_cobol_file(file_path, api_provider)
        
        # Clean up uploaded file
        os.remove(file_path)
        
        if 'error' in result:
            return jsonify(result), 500
        
        return jsonify(result)
        
    except Exception as e:
        return jsonify({'error': f'Upload failed: {str(e)}'}), 500

@app.route('/api/analyze', methods=['POST'])
def analyze_code():
    """Perform quality analysis on Java code"""
    try:
        data = request.get_json()
        java_code = data.get('code', '')
        
        if not java_code:
            return jsonify({'error': 'No code provided'}), 400
        
        # Basic code quality analysis
        analysis = {
            'metrics': {
                'lines_of_code': len(java_code.split('\n')),
                'methods_count': java_code.count('public ') + java_code.count('private ') + java_code.count('protected '),
                'classes_count': java_code.count('class '),
                'imports_count': java_code.count('import '),
                'comments_count': java_code.count('//') + java_code.count('/*')
            },
            'quality_checks': [],
            'suggestions': []
        }
        
        # Quality checks
        if 'System.out.println' in java_code:
            analysis['quality_checks'].append({
                'type': 'info',
                'message': 'Found System.out.println statements - consider using a logging framework'
            })
        
        if 'try {' in java_code:
            analysis['quality_checks'].append({
                'type': 'good',
                'message': 'Good: Exception handling implemented'
            })
        
        if java_code.count('{') != java_code.count('}'):
            analysis['quality_checks'].append({
                'type': 'warning',
                'message': 'Warning: Mismatched braces detected'
            })
        
        # Suggestions
        if 'String' in java_code and 'StringBuilder' not in java_code:
            analysis['suggestions'].append('Consider using StringBuilder for string concatenation in loops')
        
        if 'public class' in java_code and 'package ' not in java_code:
            analysis['suggestions'].append('Consider adding a package declaration')
        
        return jsonify(analysis)
        
    except Exception as e:
        return jsonify({'error': f'Analysis failed: {str(e)}'}), 500

@app.route('/api/optimize', methods=['POST'])
def optimize_code():
    """Suggest optimizations for Java code"""
    try:
        data = request.get_json()
        java_code = data.get('code', '')
        
        if not java_code:
            return jsonify({'error': 'No code provided'}), 400
        
        optimizations = {
            'performance': [],
            'readability': [],
            'best_practices': []
        }
        
        # Performance optimizations
        if 'ArrayList' in java_code and 'new ArrayList()' in java_code:
            optimizations['performance'].append({
                'issue': 'ArrayList without initial capacity',
                'suggestion': 'Specify initial capacity if size is known: new ArrayList<>(expectedSize)',
                'impact': 'Reduces memory allocations and improves performance'
            })
        
        if 'String +' in java_code:
            optimizations['performance'].append({
                'issue': 'String concatenation in loops',
                'suggestion': 'Use StringBuilder for multiple string concatenations',
                'impact': 'Significantly improves performance for multiple concatenations'
            })
        
        # Readability improvements
        if java_code.count('\n') > 50 and 'private ' not in java_code:
            optimizations['readability'].append({
                'issue': 'Large class without private methods',
                'suggestion': 'Break down large methods into smaller, private helper methods',
                'impact': 'Improves code maintainability and readability'
            })
        
        # Best practices
        if 'public static void main' in java_code and 'Scanner' in java_code:
            optimizations['best_practices'].append({
                'issue': 'Resource management',
                'suggestion': 'Use try-with-resources for Scanner and other closeable resources',
                'impact': 'Prevents resource leaks and follows Java best practices'
            })
        
        return jsonify(optimizations)
        
    except Exception as e:
        return jsonify({'error': f'Optimization failed: {str(e)}'}), 500

@app.route('/api/future-integration', methods=['POST'])
def future_integration():
    """Placeholder for future agentic integration"""
    return jsonify({
        'message': 'Future Agentic Integration',
        'features': [
            'AI-powered code review agents',
            'Automated testing generation',
            'Smart refactoring suggestions',
            'Code pattern recognition',
            'Performance monitoring agents'
        ],
        'status': 'Coming Soon',
        'description': 'This feature will integrate advanced AI agents for enhanced code analysis and automation.'
    })

@app.errorhandler(413)
def too_large(e):
    return jsonify({'error': 'File too large. Maximum size is 16MB.'}), 413

@app.errorhandler(500)
def internal_error(e):
    return jsonify({'error': 'Internal server error'}), 500

if __name__ == '__main__':
    print("🚀 Legacy Lift AI Web Application Starting...")
    print("📁 Upload folder:", app.config['UPLOAD_FOLDER'])
    print("📁 Output folder:", app.config['OUTPUT_FOLDER'])
    print("🌐 Access the application at: http://localhost:5000")
    
    app.run(debug=True, host='0.0.0.0', port=5000)
