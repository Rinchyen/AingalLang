import sys
import os
from antlr4 import *
from io import StringIO

# Add grammar path
grammar_path = os.path.join(os.path.dirname(__file__), '..', 'grammar')
sys.path.append(grammar_path)

from AingalLangLexer import AingalLangLexer
from AingalLangParser import AingalLangParser
from errorlistener import AingalLangErrorListener
from interpreter import Interpreter, InterpreterError

def main():
    debug_mode = False
    
    # Look for .aingal files in the same directory
    input_dir = os.path.dirname(__file__)
    aingal_files = [f for f in os.listdir(input_dir) if f.endswith('.aingal')]
    
    if not aingal_files:
        print("Error: No .aingal files found in directory")
        return
    
    input_file_path = os.path.join(input_dir, aingal_files[0]) 
    
    output_path = os.path.splitext(input_file_path)[0] + '.txt'

    with open(input_file_path, 'r', encoding='utf-8') as f:
        code = f.read()

    input_stream = InputStream(code)

    lexer = AingalLangLexer(input_stream)
    lexer.removeErrorListeners()
    lexer.addErrorListener(AingalLangErrorListener(code))  

    token_stream = CommonTokenStream(lexer)

    parser = AingalLangParser(token_stream)
    parser.removeErrorListeners()
    parser.addErrorListener(AingalLangErrorListener(code))  

    try:
        tree = parser.program()
        visitor = Interpreter(debug=debug_mode)
        output = visitor.visit(tree)
    except SyntaxError as e:
        print(e)
        return
    except InterpreterError as e:
        # Get the line and column from the parse tree if available
        if hasattr(e, 'ctx') and e.ctx is not None:
            line = e.ctx.start.line
            column = e.ctx.start.column
            e.line = line
            e.column = column
            e.code_line = code.split('\n')[line-1] if line <= len(code.split('\n')) else None
        print(e)
        return
    except Exception as e:
        print(f"❌ Unexpected error: {str(e)}")
        return
    
    # Write output to file
    with open(output_path, 'w', encoding='utf-8') as out_file:
        if isinstance(output, list):
            out_file.write("\n".join(str(line) for line in output))
        else:
            out_file.write(str(output))

    print(f"\nParsed successfully. Output written to {output_path}")

if __name__ == '__main__':
    main()
