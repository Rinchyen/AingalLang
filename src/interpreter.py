
from antlr4 import *
from AingalLangLexer import AingalLangLexer
from AingalLangParser import AingalLangParser
from AingalLangParserVisitor import AingalLangParserVisitor
import math


class BreakStatement:
    pass

class FunctionReturn(Exception):
    def __init__(self, value):
        self.value = value

class InterpreterError(Exception):
    def __init__(self, message, line=None, column=None, code_line=None, suggestion=None):
        self.message = message
        self.line = line
        self.column = column
        self.code_line = code_line
        self.suggestion = suggestion
        self.skip_next_block_scope = False
        
    def __str__(self):
        parts = [f"❌ Runtime Error: {self.message}"]
        if self.line is not None:
            parts.append(f"❌ At line {self.line}")
        if self.code_line is not None:
            parts.append(f"❌ Code: {self.code_line}")
            if self.column is not None:
                pointer = " " * self.column + "^"
                parts.append(f"❌        {pointer}")
        if self.suggestion is not None:
            parts.append(f"❌ Suggestion: {self.suggestion}")
        return "\n".join(parts)

class Scope:
    def __init__(self, parent=None):
        self.variables = {}
        self.parameters = set()
        self.parent = parent

    def set_variable(self, name, value, is_param=False):
        if is_param:
            self.parameters.add(name)
        self.variables[name] = value

    def has_variable(self, name):
        return name in self.variables

    def is_parameter(self, name):
        if name in self.parameters:
            return True
        if self.parent:
            return self.parent.is_parameter(name)
        return False

    def get_variable(self, name):
        if name in self.variables:
            return self.variables[name]
        elif self.parent:
            return self.parent.get_variable(name)
        else:
            raise Exception(f"Variable '{name}' not found in any scope")

class Interpreter(AingalLangParserVisitor):
    def __init__(self, debug=False):
        super().__init__()
        self.global_scope = Scope()
        self.current_scope = self.global_scope
        self.memory = {}
        self.variables = {}
        self.functions = {}        
        self.output_lines = []
        self.call_stack = []
        self.debug = debug 
        self.skip_next_block_scope = False


    def push_env(self):
        self.current_scope = Scope(parent=self.current_scope)

    def pop_env(self):
        if self.current_scope.parent:
            self.current_scope = self.current_scope.parent
        else:
            raise Exception("Cannot pop global scope")

    def set_var(self, name, value):
        scope = self.global_scope
        while scope:
            if scope.has_variable(name):
                scope.set_variable(name, value)
                return
            scope = scope.parent
        self.global_scope.set_variable(name, value)

    def get_var(self, name):
        return self.current_scope.get_variable(name)

    def visitProgram(self, ctx):
        for stmt in ctx.statement():
            result = self.visit(stmt)
            if result is not None and not isinstance(result, BreakStatement):
                self.output_lines.append(str(result))
        return self.output_lines
    
    def get_source_line(self, ctx):
        """Helper to get the source line text for error messages"""
        token = ctx.start
        input_stream = token.getInputStream()
        if input_stream:
            lines = input_stream.strdata.split('\n')
            if 0 <= token.line-1 < len(lines):
                return lines[token.line-1]
        return None

    def visitVariableDeclarationOrAssignment(self, ctx):
        # Check if this is a declaration (has SET keyword)
        is_declaration = ctx.SET() is not None

        # Get the target identifier
        if ctx.scopedIdentifier():
            # Scoped identifier handling remains the same
            scoped_name = ctx.scopedIdentifier().getText()
            value = self.visit(ctx.expression())
            type_ctx = ctx.typeAnnotation()
            declared_type = type_ctx.getText().lower() if type_ctx else None

            if declared_type:
                value = self.cast_value(value, declared_type)
            elif isinstance(value, float) and value.is_integer():
                value = int(value)

            scope, var_name = self.resolve_scope_for_assignment(scoped_name)

            if is_declaration and self._variable_exists_in_child_scopes(scope, var_name):
                line = ctx.start.line
                column = ctx.start.column
                code_line = self.get_source_line(ctx)
                raise InterpreterError(
                    message=f"Cannot declare variable '{var_name}' in parent scope when it exists in child scope",
                    line=line,
                    column=column,
                    code_line=code_line,
                    suggestion="Remove the local declaration first or use a different name"
                )

            scope.set_variable(var_name, value)

        else:
            # Regular identifier
            name = ctx.IDENTIFIER().getText()
            value = self.visit(ctx.expression())
            type_ctx = ctx.typeAnnotation()
            declared_type = type_ctx.getText().lower() if type_ctx else None

            # For declarations, check if variable exists in current scope
            in_for_init = (
                ctx.parentCtx and isinstance(ctx.parentCtx, AingalLangParser.ForInitContext)
            )
            in_for_update = (
                ctx.parentCtx and isinstance(ctx.parentCtx, AingalLangParser.ForUpdateContext)
            )

            if is_declaration:
                if not (in_for_init or in_for_update):
                    if self.current_scope.is_parameter(name):
                        line = ctx.start.line
                        column = ctx.start.column
                        code_line = self.get_source_line(ctx)
                        raise InterpreterError(
                            message=f"Cannot redeclare parameter '{name}' in this scope",
                            line=line,
                            column=column,
                            code_line=code_line,
                            suggestion="Parameters cannot be redeclared in the same function scope"
                        )

                    if self.current_scope.has_variable(name):
                        raise InterpreterError(
                            message=f"Variable '{name}' is already declared in this scope",
                            line=ctx.start.line,
                            column=ctx.start.column,
                            code_line=self.get_source_line(ctx),
                            suggestion="Use reassignment syntax instead of redeclaration"
                        )

                # For declarations, set the variable in current scope
                if declared_type:
                    value = self.cast_value(value, declared_type)
                elif isinstance(value, float) and value.is_integer():
                    value = int(value)
                self.current_scope.set_variable(name, value)

            else:
                # For reassignments, search up the scope chain
                scope = self.current_scope
                while scope and not scope.has_variable(name):
                    scope = scope.parent

                if scope is None:
                    raise InterpreterError(
                        message=f"Variable '{name}' not declared in this scope",
                        line=ctx.start.line,
                        column=ctx.start.column,
                        code_line=self.get_source_line(ctx),
                        suggestion="Declare the variable with 'Set' before assigning to it"
                    )

                if declared_type:
                    value = self.cast_value(value, declared_type)

                scope.set_variable(name, value)

        return None

    
    def _variable_exists_in_child_scopes(self, scope, name):
        """Check if variable exists in any child scope of the given scope"""
        current = self.current_scope
        while current and current != scope:
            if current.has_variable(name):
                return True
            current = current.parent
        return False


    def resolve_scope_for_assignment(self, scoped_name):
        parts = scoped_name.split("::")
        var_name = parts[-1]
        levels = len(parts) - 1

        scope = self.current_scope

        # Traverse up the required number of parent levels
        for _ in range(levels):
            if scope.parent is None:
                raise Exception(f"No parent scope exists while resolving assignment to '{scoped_name}'")
            scope = scope.parent

        # ✅ Do NOT restrict writing even if shadowed by a parameter
        return scope, var_name

    def lookup_variable(self, name):
        return self.current_scope.get_variable(name)

    def visitFunctionDeclaration(self, ctx):
        name = ctx.IDENTIFIER().getText()

        if name in self.functions:
            line = ctx.start.line
            column = ctx.start.column
            code_line = self.get_source_line(ctx)
            raise InterpreterError(
                message=f"Duplicate function declaration '{name}'",
                line=line,
                column=column,
                code_line=code_line,
                suggestion="Function names must be unique"
            )

        param_list = []
        seen_params = set()

        if ctx.parameter():
            for typed_param in ctx.parameter().typedParameter():
                param_name = typed_param.IDENTIFIER().getText()
                
                if param_name in seen_params:
                    line = typed_param.start.line
                    column = typed_param.start.column
                    code_line = self.get_source_line(typed_param)
                    raise InterpreterError(
                        message=f"Duplicate parameter name '{param_name}'",
                        line=line,
                        column=column,
                        code_line=code_line,
                        suggestion="Parameter names must be unique within a function"
                    )
                
                seen_params.add(param_name)
                param_list.append(param_name)

        body = ctx.blockStatement()
        self.functions[name] = {
            "params": param_list,
            "body": body,
            "scope": self.current_scope
        }

        return None

    def visitFunctionCall(self, ctx):

        func_name = ctx.IDENTIFIER().getText()
        args = []

        if ctx.argumentList():
            for expr in ctx.argumentList().expression():
                arg_val = self.visit(expr)
                args.append(arg_val)

        result = self.callFunction(func_name, args)
        return result      

    def callFunction(self, name, args):
        func = self.functions[name]
        param_names = func["params"]
        body = func["body"]
        defining_scope = func["scope"]

        if len(param_names) != len(args):
            raise Exception(f"Function '{name}' expects {len(param_names)} args, got {len(args)}")

        # Create local scope with the defining scope as parent
        local_scope = Scope(parent=defining_scope)

        for pname, arg in zip(param_names, args):
            local_scope.set_variable(pname, arg, is_param=True)

        # Prepare for function call
        previous_scope = self.current_scope
        self.current_scope = local_scope
        self.call_stack.append({
            'name': name,
            'params': param_names,
            'scope': local_scope
        })

        try:
            # ✅ Tell visitBlockStatement to skip its own scope pushing
            self.skip_next_block_scope = True
            result = self.visit(body)
        except FunctionReturn as fr:
            result = fr.value
        finally:
            self.call_stack.pop()
            self.current_scope = previous_scope

        return result

    
    def visitBuiltInFunctions(self, ctx):
        if ctx.POWER_FUNC():
            base = self.visit(ctx.numExpression(0))
            exponent = self.visit(ctx.numExpression(1))
            return math.pow(base, exponent)

        elif ctx.SIN_FUNC():
            value = self.visit(ctx.numExpression(0))
            return math.sin(value)

        elif ctx.COS_FUNC():
            value = self.visit(ctx.numExpression(0))
            return math.cos(value)

        elif ctx.TAN_FUNC():
            value = self.visit(ctx.numExpression(0))
            return math.tan(value)

        elif ctx.CTAN_FUNC():
            value = self.visit(ctx.numExpression(0))
            tan_val = math.tan(value)
            if tan_val == 0:
                raise ZeroDivisionError("Cotangent undefined: tan(value) = 0")
            return 1 / tan_val

        else:
            raise Exception("Unknown built-in function")


    def visitIdentifier(self, ctx):
        var_name = ctx.getText()
        if var_name in self.variables:
            return self.variables[var_name]
        else:
            raise Exception(f"Undefined variable: {var_name}")
        
    def visitScopedIdentifier(self, ctx):
        levels = len(ctx.getTokens(AingalLangParser.PARENT_SCOPE))
        name = ctx.IDENTIFIER().getText()

        current_scope = self.current_scope

        # Traverse the required levels of parent scopes
        for _ in range(levels):
            if current_scope.parent is None:
                line = ctx.start.line
                column = ctx.start.column
                code_line = self.get_source_line(ctx)
                raise InterpreterError(
                    message=f"No parent scope exists while resolving 'parent::{name}'",
                    line=line,
                    column=column,
                    code_line=code_line,
                    suggestion="Too many 'parent::' references, the parent scope doesn't exist"
                )
            current_scope = current_scope.parent

        # After reaching correct parent scope, get variable
        return current_scope.get_variable(name)

    
    def visitBlockStatement(self, ctx):
        if self.skip_next_block_scope:
            self.skip_next_block_scope = False  # reset the flag
            for stmt in ctx.statement():
                self.visit(stmt)
            return  # DO NOT push/pop

        self.push_env()
        for stmt in ctx.statement():
            self.visit(stmt)
        self.pop_env()

    
    def visitReturnStatement(self, ctx):
        value = self.visit(ctx.expression())
        raise FunctionReturn(value)

    def visitStatement(self, ctx):
        result = None
        if ctx.variableDeclarationOrAssignment():
            result = self.visit(ctx.variableDeclarationOrAssignment())
        elif ctx.functionDeclaration():
            result = self.visit(ctx.functionDeclaration())
        elif ctx.functionCall():
            result = self.visit(ctx.functionCall())
        elif ctx.returnStatement():
            result = self.visit(ctx.returnStatement())
        elif ctx.displayStatement():
            result = self.visit(ctx.displayStatement())
        elif ctx.ifStatement():
            result = self.visit(ctx.ifStatement())
        elif ctx.loopStatement():
            result = self.visit(ctx.loopStatement())
        elif ctx.forLoop():
            result = self.visit(ctx.forLoop())
        elif ctx.whileLoop():
            result = self.visit(ctx.whileLoop())
        elif ctx.blockStatement():
            result = self.visit(ctx.blockStatement())
        elif ctx.operation():
            result = self.visit(ctx.operation())

        # Handle standalone function call results
        if ctx.functionCall() and result is not None:
            self.output_lines.append(f"Result: {result}")

        return result

    def visitReassignment(self, ctx):
        name = ctx.leftHandSide().getText()
        value = self.visit(ctx.expression())

        scope = self.current_scope
        while scope:
            if scope.has_variable(name):
                current_value = scope.get_variable(name)
                if ctx.ADD_TO():
                    scope.set_variable(name, current_value + value)
                elif ctx.SUBTRACT_FROM():
                    scope.set_variable(name, current_value - value)
                elif ctx.TIMES():
                    scope.set_variable(name, current_value * value)
                elif ctx.DIVIDE_FROM():
                    scope.set_variable(name, current_value / value)
                return scope.get_variable(name)
            scope = scope.parent
        
        raise Exception(f"Variable '{name}' not defined.")

    def visitDisplayStatement(self, ctx):
        expressions = ctx.expression()
        results = [self.visit(expr) for expr in expressions]
        display_output = ' '.join(str(r) for r in results)
        if self.debug:
            print("DEBUG: Displaying", results)
        self.output_lines.append(display_output)
        print(display_output)
        return None

    def visitNumExpression(self, ctx):
        if ctx.getChildCount() == 3:
            left = self.visit(ctx.getChild(0))
            op = ctx.getChild(1).getText()
            right = self.visit(ctx.getChild(2))

            if op == '+':
                if isinstance(left, str) or isinstance(right, str):
                    return str(left) + str(right)
                else:
                    return left + right
            elif op == '-':
                if isinstance(left, (int, float)) and isinstance(right, (int, float)):
                    return left - right
                else:
                    raise TypeError(f"Unsupported operand types for -: {type(left)} and {type(right)}")
        return self.visit(ctx.getChild(0))

    def visitTerm(self, ctx):
        if ctx.getChildCount() == 3:
            left = self.visit(ctx.getChild(0))
            op = ctx.getChild(1).getText()
            right = self.visit(ctx.getChild(2))
            if op == '*':
                return left * right
            elif op == '/':
                # Perform integer division if right operand is integer literal
                if isinstance(right, int) and not isinstance(left, float):
                    return left // right
                return left / right
            elif op == '%':
                return left % right
        return self.visit(ctx.getChild(0))


    def visitUnaryPlus(self, ctx):
        return +self.visit(ctx.factor())

    def visitUnaryMinus(self, ctx):
        return -self.visit(ctx.factor())

    def visitFactorNumber(self, ctx):
        text = ctx.NUMBER().getText()
        if '.' in text:
            return float(text)
        return int(text)

    def visitFactorIdentifier(self, ctx):
        name = ctx.IDENTIFIER().getText()
        return self.lookup_variable(name)

    def visitFactorString(self, ctx):
        return ctx.STRING().getText().strip('"')

    def visitFactorParens(self, ctx):
        return self.visit(ctx.numExpression())

    def visitFactorFunctionCall(self, ctx):
        return self.visit(ctx.functionCall())

    def visitFactorOperation(self, ctx):
        return self.visit(ctx.operation())
    
    def visitFactorscopedIdentifier(self, ctx):
        return self.visit(ctx.scopedIdentifier())

    def visitOperation(self, ctx):
        var_name = ctx.IDENTIFIER().getText()
        if var_name not in self.memory:
            raise Exception(f"Variable '{var_name}' not defined.")

        current_value = self.memory[var_name]
        if not isinstance(current_value, (int, float)):
            raise Exception(f"Cannot increment/decrement non-numeric variable '{var_name}'.")

        if ctx.INCREMENT():
            self.memory[var_name] = current_value + 1
        elif ctx.DECREMENT():
            self.memory[var_name] = current_value - 1

        return self.memory[var_name]
    
    def visitNumComparison(self, ctx):
        left = self.visit(ctx.numExpression(0))
        right = self.visit(ctx.numExpression(1))
        op = ctx.comparisonOp().getText()
        return self.evaluateBinaryOp(left, right, op)

    def visitStringComparison(self, ctx):
        left = self.visit(ctx.stringExpression(0))
        right = self.visit(ctx.stringExpression(1))
        op = ctx.getChild(1).getText()
        return (left == right) if op == "==" else (left != right)

    def visitMatrixComparison(self, ctx):
        left = self.visit(ctx.matrixExpression(0))
        right = self.visit(ctx.matrixExpression(1))
        op = ctx.getChild(1).getText()
        return (left == right) if op == "==" else (left != right)

    def transpose_matrix(self, matrix):
        if not matrix or not matrix[0]:
            return []
        return [[matrix[j][i] for j in range(len(matrix))] for i in range(len(matrix[0]))]

    def invert_matrix(self, matrix):
        if len(matrix) != 2 or len(matrix[0]) != 2:
            raise Exception("Only 2x2 matrix inversion supported")

        a, b = matrix[0]
        c, d = matrix[1]
        determinant = a * d - b * c

        if determinant == 0:
            raise Exception("Matrix is not invertible")

        inverse = [
            [d / determinant, -b / determinant],
            [-c / determinant, a / determinant]
        ]
        return inverse

    def visitMatrixExpression(self, ctx):
        matrix = self.visit(ctx.matrixAtom())

        if ctx.INVERT_MATRIX():
            return self.invert_matrix(matrix)

        if ctx.TRANSPOSITION():
            return self.transpose_matrix(matrix)

        return matrix

    def visitMatrixAtom(self, ctx):
        if ctx.IDENTIFIER():
            return self.lookup_variable(ctx.IDENTIFIER().getText())
        else:
            return self.visit(ctx.matrixConstruction())

    def visitMatrixConstruction(self, ctx):
        rows = []
        for rowCtx in ctx.row():
            row = [self.visit(value) for value in rowCtx.value()]
            rows.append(row)
            print("DEBUG row:", row)
        return rows

    def visitValue(self, ctx):
        if ctx.NUMBER():
            return float(ctx.NUMBER().getText())
        elif ctx.IDENTIFIER():
            value = self.lookup_variable(ctx.IDENTIFIER().getText())
            if isinstance(value, list) and any(isinstance(row, list) for row in value):
                raise Exception("Matrix cannot contain another matrix as an element")
            return value
        elif ctx.matrixExpression():
            raise Exception("Matrix cannot contain another matrix as an element")
        else:
            raise Exception("Invalid matrix value: must be a number or scalar variable")

    def visitBoolExpression(self, ctx):
        return self.visitChildren(ctx)
    
    def visitCastExpression(self, ctx):
        target_type = ctx.typeAnnotation().getText().lower()
        value = self.visit(ctx.factor())
        
        if target_type == 'int':
            if isinstance(value, bool):
                return 1 if value else 0
            try:
                return int(float(value))
            except ValueError:
                raise Exception(f"Cannot cast {value} to int")
        elif target_type == 'float':
            try:
                return float(value)
            except ValueError:
                raise Exception(f"Cannot cast {value} to float")
        elif target_type == 'bool':
            if isinstance(value, str):
                return value.lower() == 'true'
            return bool(value)
        elif target_type == 'string':
            return str(value)
        else:
            raise Exception(f"Unsupported cast type: {target_type}")
        
    def cast_value(self, value, type_str):
        if type_str is None:
            if isinstance(value, float) and value.is_integer():
                return int(value)
            return value
        if type_str == "int":
            return int(float(value))  
        elif type_str == "float":
            return float(value)
        elif type_str == "bool":
            if isinstance(value, str):
                return value.lower() == "true"
            return bool(value)
        elif type_str == "string":
            return str(value)
        elif type_str == "matrix":
            if isinstance(value, list) and all(isinstance(row, list) for row in value):
                return value
            raise Exception("Invalid matrix format")
        else:
            raise Exception(f"Unknown type: {type_str}")

    def visitLogicOr(self, ctx):
        for i in range(len(ctx.boolAndExpression())):
            if self.visit(ctx.boolAndExpression(i)):
                return True
        return False

    def visitLogicAnd(self, ctx):
        for i in range(len(ctx.boolNotExpression())):
            if not self.visit(ctx.boolNotExpression(i)):
                return False
        return True

    def visitLogicNot(self, ctx):
        return not self.visit(ctx.boolNotExpression())

    def visitLogicPrimaryWrap(self, ctx):
        return self.visit(ctx.boolPrimary())

    def visitLogicParen(self, ctx):
        return self.visit(ctx.boolExpression())

    def visitTrueLiteral(self, ctx):
        return True

    def visitFalseLiteral(self, ctx):
        return False
    
    def visitFactorTrue(self, ctx):
        return True

    def visitFactorFalse(self, ctx):
        return False

    def visitLogicIdentifier(self, ctx):
        name = ctx.IDENTIFIER().getText()
        return bool(self.lookup_variable(name))

    def visitIfStatement(self, ctx):
        if self.visit(ctx.boolExpression(0)):
            stmt_or_block = ctx.statement(0) or ctx.blockStatement(0)
            if stmt_or_block:
                return self.visit(stmt_or_block)

        for i in range(len(ctx.ELSE_IF())):
            if self.visit(ctx.boolExpression(i + 1)):
                stmt_or_block = ctx.statement(i + 1) or ctx.blockStatement(i + 1)
                if stmt_or_block:
                    return self.visit(stmt_or_block)

        if ctx.ELSE():
            num_ifs = 1 + len(ctx.ELSE_IF())
            stmt_or_block = ctx.statement(num_ifs) or ctx.blockStatement(num_ifs)
            if stmt_or_block:
                return self.visit(stmt_or_block)

        return None


    def visitLoopIfStatement(self, ctx):
        if self.visit(ctx.boolExpression(0)):
            if ctx.loopStatements(0):
                return self.visit(ctx.loopStatements(0))
            elif ctx.statement(0):
                return self.visit(ctx.statement(0))

        for i in range(1, len(ctx.boolExpression())):
            if self.visit(ctx.boolExpression(i)):
                if ctx.loopStatements(i):
                    return self.visit(ctx.loopStatements(i))
                elif ctx.statement(i):
                    return self.visit(ctx.statement(i))

        if ctx.ELSE(): 
            idx = len(ctx.boolExpression())
            if len(ctx.loopStatements()) > idx:
                return self.visit(ctx.loopStatements(idx))
            elif len(ctx.statement()) > idx:
                return self.visit(ctx.statement(idx))

        return None

    def visitBreakStatement(self, ctx):
        return BreakStatement()
    
    def visitLoopStatements(self, ctx):
        if ctx.loopStatement():
            return self.visit(ctx.loopStatement())
        elif ctx.variableDeclarationOrAssignment():
            return self.visit(ctx.variableDeclarationOrAssignment())
        elif ctx.functionDeclaration():
            return self.visit(ctx.functionDeclaration())
        elif ctx.returnStatement():
            return self.visit(ctx.returnStatement())
        elif ctx.ifStatement():
            return self.visit(ctx.ifStatement())
        elif ctx.blockStatement():
            return self.visit(ctx.blockStatement())
        elif ctx.displayStatement():
            return self.visit(ctx.displayStatement())

    def visitWhileLoop(self, ctx):
        while self.visit(ctx.boolExpression()):
            self.push_env()  

            for stmt in ctx.loopStatements():
                result = self.visit(stmt)
                if isinstance(result, BreakStatement):
                    self.pop_env()
                    return
                if result is not None:
                    self.pop_env()
                    return result

            self.pop_env()  
        return None

            
    def visitForLoop(self, ctx):
        if ctx.forInit():
            self.visit(ctx.forInit())

        while self.visit(ctx.cond):
            self.push_env()
            result = self.visit(ctx.forBody())
            if isinstance(result, BreakStatement):
                self.pop_env()
                return
            self.pop_env()

            if self.visit(ctx.cond):
                self.visit(ctx.forUpdate())



    def evaluateBinaryOp(self, left, right, op):
        if op == '+': return left + right
        elif op == '-': return left - right
        elif op == '*': return left * right
        elif op == '/': return left / right
        elif op == '%': return left % right
        elif op == '<': return left < right
        elif op == '>': return left > right
        elif op == '==': return left == right
        elif op == '!=': return left != right
        elif op == '<=': return left <= right
        elif op == '>=': return left >= right
        else: raise Exception(f"Unknown operator {op}")

    def visitExpression(self, ctx):
        if ctx.numExpression():
            return self.visit(ctx.numExpression())
        elif ctx.boolExpression():
            return self.visit(ctx.boolExpression())
        elif ctx.functionCall():
            return self.visit(ctx.functionCall())
        elif ctx.builtInFunctions():
            return self.visit(ctx.builtInFunctions())
        elif ctx.matrixExpression():
            return self.visit(ctx.matrixExpression())
        elif ctx.stringExpression():
            return self.visit(ctx.stringExpression())
        elif ctx.scopedIdentifier():
            return self.visit(ctx.scopedIdentifier())
        elif ctx.STRING():
            return ctx.STRING().getText().strip('"')
        elif ctx.IDENTIFIER():
            name = ctx.IDENTIFIER().getText()
            return self.lookup_variable(name)
        return None
