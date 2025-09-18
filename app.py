#!/usr/bin/env python3
"""
Legacy Lift AI - Web Application
COBOL to Java Conversion Service
"""

from flask import Flask, request, jsonify, render_template, send_from_directory, redirect, url_for
from werkzeug.utils import secure_filename
import os
import json
import tempfile
import shutil
from pathlib import Path
import subprocess
import sys

# Import the updated direct conversion module
from ast_to_java_converter import convert_cobol_to_java

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
        
    def process_cobol_file(self, file_path, api_provider='gemini'):
        """Process COBOL file using direct Gemini API conversion"""
        try:
            # Check if Gemini API key is available
            if not os.getenv("GEMINI_API_KEY"):
                return {'error': 'GEMINI_API_KEY environment variable is required. Please set your Gemini API key.'}
            
            # Read the COBOL file
            with open(file_path, 'r', encoding='utf-8') as f:
                cobol_code = f.read()
            
            if not cobol_code.strip():
                return {'error': 'COBOL file is empty'}
            
            # Extract program name from file
            base_name = Path(file_path).stem
            
            # Convert COBOL to Java using direct Gemini API
            java_code = convert_cobol_to_java(cobol_code, base_name)
            
            # Check if conversion was successful
            if java_code.startswith('Error'):
                return {'error': java_code}
            
            # Save the generated Java code
            java_file_path = os.path.join(app.config['OUTPUT_FOLDER'], f"{base_name}.java")
            with open(java_file_path, 'w', encoding='utf-8') as f:
                f.write(java_code)
            
            # Analyze the COBOL code to provide stats
            cobol_lines = cobol_code.split('\n')
            divisions_found = []
            for line in cobol_lines:
                line = line.strip().upper()
                if 'IDENTIFICATION DIVISION' in line:
                    divisions_found.append('Identification')
                elif 'ENVIRONMENT DIVISION' in line:
                    divisions_found.append('Environment')
                elif 'DATA DIVISION' in line:
                    divisions_found.append('Data')
                elif 'PROCEDURE DIVISION' in line:
                    divisions_found.append('Procedure')
            
            return {
                'success': True,
                'java_code': java_code,
                'java_file_path': java_file_path,
                'cobol_analysis': {
                    'total_lines': len(cobol_lines),
                    'divisions_found': divisions_found,
                    'program_name': base_name
                },
                'stats': {
                    'divisions_converted': len(divisions_found) if divisions_found else 1,
                    'failed_conversions': 0,
                    'api_provider': 'gemini',
                    'conversion_method': 'direct_api'
                }
            }
            
        except Exception as e:
            return {'error': f'Processing failed: {str(e)}'}

processor = COBOLProcessor()

@app.route('/')
def home():
    return render_template('home.html')

@app.route('/converter')
def converter():
    return render_template('converter.html')

@app.route('/index')
def index():
    # Redirect old index route to converter for backward compatibility
    return redirect('/converter')

@app.route('/api/upload', methods=['POST'])
def upload_file():
    """Handle COBOL file upload and direct Gemini conversion"""
    try:
        print("🔍 Upload endpoint called")
        
        if 'file' not in request.files:
            print("❌ No file in request")
            return jsonify({'error': 'No file provided'}), 400
        
        file = request.files['file']
        if file.filename == '':
            print("❌ Empty filename")
            return jsonify({'error': 'No file selected'}), 400
        
        if not allowed_file(file.filename):
            print(f"❌ Invalid file type: {file.filename}")
            return jsonify({'error': 'Invalid file type. Please upload a COBOL file (.cobol, .cob, .cbl, .txt)'}), 400
        
        # Check if Gemini API key is available
        api_key = os.getenv("GEMINI_API_KEY")
        if not api_key:
            print("❌ No GEMINI_API_KEY found in environment")
            return jsonify({'error': 'GEMINI_API_KEY environment variable is required. Please set your Gemini API key in .env file'}), 400
        
        print(f"✅ API key found: {api_key[:10]}...")
        
        # Read COBOL code directly from uploaded file
        cobol_code = file.read().decode('utf-8')
        print(f"📄 COBOL code read: {len(cobol_code)} characters")
        
        if not cobol_code.strip():
            print("❌ COBOL file is empty")
            return jsonify({'error': 'COBOL file is empty'}), 400
        
        # Extract program name from filename
        program_name = Path(file.filename).stem
        print(f"🏷️ Program name: {program_name}")
        
        # Convert COBOL to Java using direct Gemini API
        print("🤖 Starting Gemini API conversion...")
        from ast_to_java_converter import convert_cobol_to_java
        java_code = convert_cobol_to_java(cobol_code, program_name)
        print(f"✅ Conversion completed: {len(java_code)} characters")
        
        # Check if conversion was successful
        if java_code.startswith('Error'):
            print(f"❌ Conversion error: {java_code}")
            return jsonify({'error': java_code}), 500
        
        # Save the generated Java code
        java_file_path = os.path.join(app.config['OUTPUT_FOLDER'], f"{program_name}.java")
        with open(java_file_path, 'w', encoding='utf-8') as f:
            f.write(java_code)
        print(f"💾 Java code saved to: {java_file_path}")
        
        result = {
            'success': True,
            'java_code': java_code,
            'java_file_path': java_file_path,
            'program_name': program_name,
            'stats': {
                'cobol_lines': len(cobol_code.split('\n')),
                'java_lines': len(java_code.split('\n')),
                'api_provider': 'gemini',
                'conversion_method': 'direct_api'
            }
        }
        
        print("🎉 Conversion successful, returning result")
        return jsonify(result)
        
    except Exception as e:
        print(f"💥 Exception in upload_file: {str(e)}")
        import traceback
        traceback.print_exc()
        return jsonify({'error': f'Upload failed: {str(e)}'}), 500

@app.route('/api/compile', methods=['POST'])
def compile_java():
    """Compile the generated Java code"""
    try:
        data = request.get_json()
        java_code = data.get('java_code', '')
        
        if not java_code:
            return jsonify({'error': 'No Java code provided'}), 400
        
        # Create temporary directory for compilation
        with tempfile.TemporaryDirectory() as temp_dir:
            # Extract class name from Java code
            class_name = "CobolProgram"
            for line in java_code.split('\n'):
                if 'public class ' in line:
                    class_name = line.split('public class ')[1].split(' ')[0].split('{')[0].strip()
                    break
            
            # Write Java code to file
            java_file = os.path.join(temp_dir, f"{class_name}.java")
            with open(java_file, 'w', encoding='utf-8') as f:
                f.write(java_code)
            
            # Compile Java code
            try:
                result = subprocess.run(
                    ['javac', java_file],
                    capture_output=True,
                    text=True,
                    timeout=30,
                    cwd=temp_dir
                )
                
                if result.returncode == 0:
                    # Compilation successful
                    class_file = os.path.join(temp_dir, f"{class_name}.class")
                    return jsonify({
                        'success': True,
                        'message': 'Java code compiled successfully!',
                        'class_name': class_name,
                        'class_file_exists': os.path.exists(class_file),
                        'run_command': f"java {class_name}"
                    })
                else:
                    # Compilation failed
                    return jsonify({
                        'success': False,
                        'error': 'Compilation failed',
                        'details': result.stderr,
                        'suggestions': [
                            'Check for syntax errors in the generated code',
                            'Ensure all imports are correct',
                            'Verify class and method declarations',
                            'Try using the "Clean Up Java" feature first'
                        ]
                    })
                    
            except subprocess.TimeoutExpired:
                return jsonify({'error': 'Compilation timed out'}), 500
            except FileNotFoundError:
                return jsonify({'error': 'Java compiler (javac) not found. Please install Java Development Kit (JDK).'}), 500
        
    except Exception as e:
        return jsonify({'error': f'Compilation failed: {str(e)}'}), 500

@app.route('/api/cleanup-java', methods=['POST'])
def cleanup_java():
    """Clean up Java code using Gemini API"""
    try:
        data = request.get_json()
        java_code = data.get('java_code', '')
        
        if not java_code:
            return jsonify({'error': 'No Java code provided'}), 400
        
        # Check if Gemini API key is available
        if not os.getenv("GEMINI_API_KEY"):
            return jsonify({'error': 'GEMINI_API_KEY environment variable is required'}), 500
        
        # Use Gemini to clean up the Java code
        from ast_to_java_converter import CobolToJavaConverter
        
        cleanup_prompt = f"""
Clean up and fix the following Java code to make it compilable and well-structured:

Issues to fix:
1. Remove duplicate class definitions
2. Consolidate imports at the top
3. Remove empty or unnecessary classes
4. Fix package declarations
5. Ensure proper Java syntax
6. Remove any scattered import statements
7. Consolidate multiple main methods if present

Java Code to Clean:
```java
{java_code}
```

Return only the cleaned Java code without any explanations or markdown formatting.
"""
        
        try:
            converter = CobolToJavaConverter()
            cleaned_code = converter._call_gemini_api(cleanup_prompt)
            
            if cleaned_code:
                cleaned_code = converter._clean_java_response(cleaned_code)
                return jsonify({
                    'success': True,
                    'cleaned_code': cleaned_code
                })
            else:
                return jsonify({'error': 'Failed to clean up Java code using Gemini API'}), 500
                
        except Exception as e:
            return jsonify({'error': f'Cleanup failed: {str(e)}'}), 500
        
    except Exception as e:
        return jsonify({'error': f'Cleanup request failed: {str(e)}'}), 500

@app.route('/api/run-java', methods=['POST'])
def run_java():
    """Execute the compiled Java program"""
    try:
        data = request.get_json()
        java_code = data.get('java_code', '')
        program_input = data.get('input', 'John Doe\n1000\n2000\n3000\nNO\n')
        
        if not java_code:
            return jsonify({'error': 'No Java code provided'}), 400
        
        # Create temporary directory for execution
        with tempfile.TemporaryDirectory() as temp_dir:
            # Extract class name from Java code
            class_name = "CobolProgram"
            for line in java_code.split('\n'):
                if 'public class ' in line:
                    class_name = line.split('public class ')[1].split(' ')[0].split('{')[0].strip()
                    break
            
            # Write Java code to file
            java_file = os.path.join(temp_dir, f"{class_name}.java")
            with open(java_file, 'w', encoding='utf-8') as f:
                f.write(java_code)
            
            try:
                # Compile first
                compile_result = subprocess.run(
                    ['javac', java_file],
                    capture_output=True,
                    text=True,
                    timeout=30,
                    cwd=temp_dir
                )
                
                if compile_result.returncode != 0:
                    return jsonify({
                        'success': False,
                        'error': 'Compilation failed before execution',
                        'details': compile_result.stderr
                    })
                
                # Run the program
                run_result = subprocess.run(
                    ['java', class_name],
                    input=program_input,
                    capture_output=True,
                    text=True,
                    timeout=30,
                    cwd=temp_dir
                )
                
                return jsonify({
                    'success': True,
                    'return_code': run_result.returncode,
                    'output': run_result.stdout,
                    'error_output': run_result.stderr,
                    'input_provided': program_input
                })
                
            except subprocess.TimeoutExpired:
                return jsonify({'error': 'Program execution timed out'}), 500
            except FileNotFoundError:
                return jsonify({'error': 'Java runtime not found. Please install Java.'}), 500
        
    except Exception as e:
        return jsonify({'error': f'Execution failed: {str(e)}'}), 500

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

# Disable caching for development
@app.after_request
def after_request(response):
    response.headers["Cache-Control"] = "no-cache, no-store, must-revalidate"
    response.headers["Pragma"] = "no-cache"
    response.headers["Expires"] = "0"
    return response

if __name__ == '__main__':
    print("🚀 Legacy Lift AI Web Application Starting...")
    print("📁 Upload folder:", app.config['UPLOAD_FOLDER'])
    print("📁 Output folder:", app.config['OUTPUT_FOLDER'])
    print("🌐 Access the application at: http://localhost:5000")
    
    app.run(debug=True, host='0.0.0.0', port=5000)
