#!/usr/bin/env python3
"""
Test Ollama connection and available models
"""

import requests
import json

def test_ollama_connection(url="http://localhost:11434"):
    """Test Ollama connection and list available models"""
    
    print(f"Testing connection to: {url}")
    
    # Test basic connection
    try:
        response = requests.get(f"{url}/api/tags", timeout=5)
        print(f"Connection status: {response.status_code}")
        
        if response.status_code == 200:
            models = response.json()
            print(f"Available models: {json.dumps(models, indent=2)}")
            
            if models.get("models"):
                return models["models"][0]["name"]  # Return first available model
            else:
                print("No models found!")
                return None
        else:
            print(f"Error response: {response.text}")
            return None
            
    except requests.exceptions.RequestException as e:
        print(f"Connection failed: {e}")
        return None

def test_generate(model_name, url="http://localhost:11434"):
    """Test text generation with a simple prompt"""
    
    payload = {
        "model": model_name,
        "prompt": "Convert this COBOL statement to Java: MOVE 'HELLO' TO GREETING.",
        "stream": False,
        "options": {
            "temperature": 0.1,
            "num_predict": 100
        }
    }
    
    try:
        response = requests.post(
            f"{url}/api/generate",
            json=payload,
            timeout=30
        )
        
        print(f"Generate API status: {response.status_code}")
        
        if response.status_code == 200:
            result = response.json()
            print(f"Generated response: {result.get('response', 'No response')}")
            return True
        else:
            print(f"Generate error: {response.text}")
            return False
            
    except requests.exceptions.RequestException as e:
        print(f"Generate request failed: {e}")
        return False

def main():
    print("🔍 Testing Ollama setup...")
    
    # Test connection and get available model
    model_name = test_ollama_connection()
    
    if model_name:
        print(f"\n🤖 Testing generation with model: {model_name}")
        success = test_generate(model_name)
        
        if success:
            print(f"\n✅ Ollama is working correctly with model: {model_name}")
            print(f"You can use this model in the converter by updating the model_name parameter")
        else:
            print(f"\n❌ Generation test failed")
    else:
        print(f"\n❌ No models available. Install a model with: ollama pull llama3.2")

if __name__ == "__main__":
    main()
