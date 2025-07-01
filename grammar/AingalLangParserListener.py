# Generated from AingalLangParser.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .AingalLangParser import AingalLangParser
else:
    from AingalLangParser import AingalLangParser

# This class defines a complete listener for a parse tree produced by AingalLangParser.
class AingalLangParserListener(ParseTreeListener):

    # Enter a parse tree produced by AingalLangParser#program.
    def enterProgram(self, ctx:AingalLangParser.ProgramContext):
        pass

    # Exit a parse tree produced by AingalLangParser#program.
    def exitProgram(self, ctx:AingalLangParser.ProgramContext):
        pass


    # Enter a parse tree produced by AingalLangParser#statement.
    def enterStatement(self, ctx:AingalLangParser.StatementContext):
        pass

    # Exit a parse tree produced by AingalLangParser#statement.
    def exitStatement(self, ctx:AingalLangParser.StatementContext):
        pass


    # Enter a parse tree produced by AingalLangParser#loopStatements.
    def enterLoopStatements(self, ctx:AingalLangParser.LoopStatementsContext):
        pass

    # Exit a parse tree produced by AingalLangParser#loopStatements.
    def exitLoopStatements(self, ctx:AingalLangParser.LoopStatementsContext):
        pass


    # Enter a parse tree produced by AingalLangParser#variableDeclarationOrAssignment.
    def enterVariableDeclarationOrAssignment(self, ctx:AingalLangParser.VariableDeclarationOrAssignmentContext):
        pass

    # Exit a parse tree produced by AingalLangParser#variableDeclarationOrAssignment.
    def exitVariableDeclarationOrAssignment(self, ctx:AingalLangParser.VariableDeclarationOrAssignmentContext):
        pass


    # Enter a parse tree produced by AingalLangParser#matrixExpression.
    def enterMatrixExpression(self, ctx:AingalLangParser.MatrixExpressionContext):
        pass

    # Exit a parse tree produced by AingalLangParser#matrixExpression.
    def exitMatrixExpression(self, ctx:AingalLangParser.MatrixExpressionContext):
        pass


    # Enter a parse tree produced by AingalLangParser#matrixAtom.
    def enterMatrixAtom(self, ctx:AingalLangParser.MatrixAtomContext):
        pass

    # Exit a parse tree produced by AingalLangParser#matrixAtom.
    def exitMatrixAtom(self, ctx:AingalLangParser.MatrixAtomContext):
        pass


    # Enter a parse tree produced by AingalLangParser#matrixConstruction.
    def enterMatrixConstruction(self, ctx:AingalLangParser.MatrixConstructionContext):
        pass

    # Exit a parse tree produced by AingalLangParser#matrixConstruction.
    def exitMatrixConstruction(self, ctx:AingalLangParser.MatrixConstructionContext):
        pass


    # Enter a parse tree produced by AingalLangParser#row.
    def enterRow(self, ctx:AingalLangParser.RowContext):
        pass

    # Exit a parse tree produced by AingalLangParser#row.
    def exitRow(self, ctx:AingalLangParser.RowContext):
        pass


    # Enter a parse tree produced by AingalLangParser#value.
    def enterValue(self, ctx:AingalLangParser.ValueContext):
        pass

    # Exit a parse tree produced by AingalLangParser#value.
    def exitValue(self, ctx:AingalLangParser.ValueContext):
        pass


    # Enter a parse tree produced by AingalLangParser#stringExpression.
    def enterStringExpression(self, ctx:AingalLangParser.StringExpressionContext):
        pass

    # Exit a parse tree produced by AingalLangParser#stringExpression.
    def exitStringExpression(self, ctx:AingalLangParser.StringExpressionContext):
        pass


    # Enter a parse tree produced by AingalLangParser#expression.
    def enterExpression(self, ctx:AingalLangParser.ExpressionContext):
        pass

    # Exit a parse tree produced by AingalLangParser#expression.
    def exitExpression(self, ctx:AingalLangParser.ExpressionContext):
        pass


    # Enter a parse tree produced by AingalLangParser#scopedIdentifier.
    def enterScopedIdentifier(self, ctx:AingalLangParser.ScopedIdentifierContext):
        pass

    # Exit a parse tree produced by AingalLangParser#scopedIdentifier.
    def exitScopedIdentifier(self, ctx:AingalLangParser.ScopedIdentifierContext):
        pass


    # Enter a parse tree produced by AingalLangParser#typeAnnotation.
    def enterTypeAnnotation(self, ctx:AingalLangParser.TypeAnnotationContext):
        pass

    # Exit a parse tree produced by AingalLangParser#typeAnnotation.
    def exitTypeAnnotation(self, ctx:AingalLangParser.TypeAnnotationContext):
        pass


    # Enter a parse tree produced by AingalLangParser#functionDeclaration.
    def enterFunctionDeclaration(self, ctx:AingalLangParser.FunctionDeclarationContext):
        pass

    # Exit a parse tree produced by AingalLangParser#functionDeclaration.
    def exitFunctionDeclaration(self, ctx:AingalLangParser.FunctionDeclarationContext):
        pass


    # Enter a parse tree produced by AingalLangParser#parameter.
    def enterParameter(self, ctx:AingalLangParser.ParameterContext):
        pass

    # Exit a parse tree produced by AingalLangParser#parameter.
    def exitParameter(self, ctx:AingalLangParser.ParameterContext):
        pass


    # Enter a parse tree produced by AingalLangParser#typedParameter.
    def enterTypedParameter(self, ctx:AingalLangParser.TypedParameterContext):
        pass

    # Exit a parse tree produced by AingalLangParser#typedParameter.
    def exitTypedParameter(self, ctx:AingalLangParser.TypedParameterContext):
        pass


    # Enter a parse tree produced by AingalLangParser#returnStatement.
    def enterReturnStatement(self, ctx:AingalLangParser.ReturnStatementContext):
        pass

    # Exit a parse tree produced by AingalLangParser#returnStatement.
    def exitReturnStatement(self, ctx:AingalLangParser.ReturnStatementContext):
        pass


    # Enter a parse tree produced by AingalLangParser#functionCall.
    def enterFunctionCall(self, ctx:AingalLangParser.FunctionCallContext):
        pass

    # Exit a parse tree produced by AingalLangParser#functionCall.
    def exitFunctionCall(self, ctx:AingalLangParser.FunctionCallContext):
        pass


    # Enter a parse tree produced by AingalLangParser#argumentList.
    def enterArgumentList(self, ctx:AingalLangParser.ArgumentListContext):
        pass

    # Exit a parse tree produced by AingalLangParser#argumentList.
    def exitArgumentList(self, ctx:AingalLangParser.ArgumentListContext):
        pass


    # Enter a parse tree produced by AingalLangParser#builtInFunctions.
    def enterBuiltInFunctions(self, ctx:AingalLangParser.BuiltInFunctionsContext):
        pass

    # Exit a parse tree produced by AingalLangParser#builtInFunctions.
    def exitBuiltInFunctions(self, ctx:AingalLangParser.BuiltInFunctionsContext):
        pass


    # Enter a parse tree produced by AingalLangParser#ifStatement.
    def enterIfStatement(self, ctx:AingalLangParser.IfStatementContext):
        pass

    # Exit a parse tree produced by AingalLangParser#ifStatement.
    def exitIfStatement(self, ctx:AingalLangParser.IfStatementContext):
        pass


    # Enter a parse tree produced by AingalLangParser#loopIfStatement.
    def enterLoopIfStatement(self, ctx:AingalLangParser.LoopIfStatementContext):
        pass

    # Exit a parse tree produced by AingalLangParser#loopIfStatement.
    def exitLoopIfStatement(self, ctx:AingalLangParser.LoopIfStatementContext):
        pass


    # Enter a parse tree produced by AingalLangParser#loopStatement.
    def enterLoopStatement(self, ctx:AingalLangParser.LoopStatementContext):
        pass

    # Exit a parse tree produced by AingalLangParser#loopStatement.
    def exitLoopStatement(self, ctx:AingalLangParser.LoopStatementContext):
        pass


    # Enter a parse tree produced by AingalLangParser#forLoop.
    def enterForLoop(self, ctx:AingalLangParser.ForLoopContext):
        pass

    # Exit a parse tree produced by AingalLangParser#forLoop.
    def exitForLoop(self, ctx:AingalLangParser.ForLoopContext):
        pass


    # Enter a parse tree produced by AingalLangParser#forInit.
    def enterForInit(self, ctx:AingalLangParser.ForInitContext):
        pass

    # Exit a parse tree produced by AingalLangParser#forInit.
    def exitForInit(self, ctx:AingalLangParser.ForInitContext):
        pass


    # Enter a parse tree produced by AingalLangParser#forUpdate.
    def enterForUpdate(self, ctx:AingalLangParser.ForUpdateContext):
        pass

    # Exit a parse tree produced by AingalLangParser#forUpdate.
    def exitForUpdate(self, ctx:AingalLangParser.ForUpdateContext):
        pass


    # Enter a parse tree produced by AingalLangParser#forBody.
    def enterForBody(self, ctx:AingalLangParser.ForBodyContext):
        pass

    # Exit a parse tree produced by AingalLangParser#forBody.
    def exitForBody(self, ctx:AingalLangParser.ForBodyContext):
        pass


    # Enter a parse tree produced by AingalLangParser#whileLoop.
    def enterWhileLoop(self, ctx:AingalLangParser.WhileLoopContext):
        pass

    # Exit a parse tree produced by AingalLangParser#whileLoop.
    def exitWhileLoop(self, ctx:AingalLangParser.WhileLoopContext):
        pass


    # Enter a parse tree produced by AingalLangParser#displayStatement.
    def enterDisplayStatement(self, ctx:AingalLangParser.DisplayStatementContext):
        pass

    # Exit a parse tree produced by AingalLangParser#displayStatement.
    def exitDisplayStatement(self, ctx:AingalLangParser.DisplayStatementContext):
        pass


    # Enter a parse tree produced by AingalLangParser#blockStatement.
    def enterBlockStatement(self, ctx:AingalLangParser.BlockStatementContext):
        pass

    # Exit a parse tree produced by AingalLangParser#blockStatement.
    def exitBlockStatement(self, ctx:AingalLangParser.BlockStatementContext):
        pass


    # Enter a parse tree produced by AingalLangParser#numExpression.
    def enterNumExpression(self, ctx:AingalLangParser.NumExpressionContext):
        pass

    # Exit a parse tree produced by AingalLangParser#numExpression.
    def exitNumExpression(self, ctx:AingalLangParser.NumExpressionContext):
        pass


    # Enter a parse tree produced by AingalLangParser#term.
    def enterTerm(self, ctx:AingalLangParser.TermContext):
        pass

    # Exit a parse tree produced by AingalLangParser#term.
    def exitTerm(self, ctx:AingalLangParser.TermContext):
        pass


    # Enter a parse tree produced by AingalLangParser#unaryPlus.
    def enterUnaryPlus(self, ctx:AingalLangParser.UnaryPlusContext):
        pass

    # Exit a parse tree produced by AingalLangParser#unaryPlus.
    def exitUnaryPlus(self, ctx:AingalLangParser.UnaryPlusContext):
        pass


    # Enter a parse tree produced by AingalLangParser#unaryMinus.
    def enterUnaryMinus(self, ctx:AingalLangParser.UnaryMinusContext):
        pass

    # Exit a parse tree produced by AingalLangParser#unaryMinus.
    def exitUnaryMinus(self, ctx:AingalLangParser.UnaryMinusContext):
        pass


    # Enter a parse tree produced by AingalLangParser#factorFunctionCall.
    def enterFactorFunctionCall(self, ctx:AingalLangParser.FactorFunctionCallContext):
        pass

    # Exit a parse tree produced by AingalLangParser#factorFunctionCall.
    def exitFactorFunctionCall(self, ctx:AingalLangParser.FactorFunctionCallContext):
        pass


    # Enter a parse tree produced by AingalLangParser#factorNumber.
    def enterFactorNumber(self, ctx:AingalLangParser.FactorNumberContext):
        pass

    # Exit a parse tree produced by AingalLangParser#factorNumber.
    def exitFactorNumber(self, ctx:AingalLangParser.FactorNumberContext):
        pass


    # Enter a parse tree produced by AingalLangParser#factorscopedIdentifier.
    def enterFactorscopedIdentifier(self, ctx:AingalLangParser.FactorscopedIdentifierContext):
        pass

    # Exit a parse tree produced by AingalLangParser#factorscopedIdentifier.
    def exitFactorscopedIdentifier(self, ctx:AingalLangParser.FactorscopedIdentifierContext):
        pass


    # Enter a parse tree produced by AingalLangParser#factorIdentifier.
    def enterFactorIdentifier(self, ctx:AingalLangParser.FactorIdentifierContext):
        pass

    # Exit a parse tree produced by AingalLangParser#factorIdentifier.
    def exitFactorIdentifier(self, ctx:AingalLangParser.FactorIdentifierContext):
        pass


    # Enter a parse tree produced by AingalLangParser#factorString.
    def enterFactorString(self, ctx:AingalLangParser.FactorStringContext):
        pass

    # Exit a parse tree produced by AingalLangParser#factorString.
    def exitFactorString(self, ctx:AingalLangParser.FactorStringContext):
        pass


    # Enter a parse tree produced by AingalLangParser#factorOperation.
    def enterFactorOperation(self, ctx:AingalLangParser.FactorOperationContext):
        pass

    # Exit a parse tree produced by AingalLangParser#factorOperation.
    def exitFactorOperation(self, ctx:AingalLangParser.FactorOperationContext):
        pass


    # Enter a parse tree produced by AingalLangParser#factorParens.
    def enterFactorParens(self, ctx:AingalLangParser.FactorParensContext):
        pass

    # Exit a parse tree produced by AingalLangParser#factorParens.
    def exitFactorParens(self, ctx:AingalLangParser.FactorParensContext):
        pass


    # Enter a parse tree produced by AingalLangParser#castExpression.
    def enterCastExpression(self, ctx:AingalLangParser.CastExpressionContext):
        pass

    # Exit a parse tree produced by AingalLangParser#castExpression.
    def exitCastExpression(self, ctx:AingalLangParser.CastExpressionContext):
        pass


    # Enter a parse tree produced by AingalLangParser#factorTrue.
    def enterFactorTrue(self, ctx:AingalLangParser.FactorTrueContext):
        pass

    # Exit a parse tree produced by AingalLangParser#factorTrue.
    def exitFactorTrue(self, ctx:AingalLangParser.FactorTrueContext):
        pass


    # Enter a parse tree produced by AingalLangParser#factorFalse.
    def enterFactorFalse(self, ctx:AingalLangParser.FactorFalseContext):
        pass

    # Exit a parse tree produced by AingalLangParser#factorFalse.
    def exitFactorFalse(self, ctx:AingalLangParser.FactorFalseContext):
        pass


    # Enter a parse tree produced by AingalLangParser#operation.
    def enterOperation(self, ctx:AingalLangParser.OperationContext):
        pass

    # Exit a parse tree produced by AingalLangParser#operation.
    def exitOperation(self, ctx:AingalLangParser.OperationContext):
        pass


    # Enter a parse tree produced by AingalLangParser#leftHandSide.
    def enterLeftHandSide(self, ctx:AingalLangParser.LeftHandSideContext):
        pass

    # Exit a parse tree produced by AingalLangParser#leftHandSide.
    def exitLeftHandSide(self, ctx:AingalLangParser.LeftHandSideContext):
        pass


    # Enter a parse tree produced by AingalLangParser#reassignment.
    def enterReassignment(self, ctx:AingalLangParser.ReassignmentContext):
        pass

    # Exit a parse tree produced by AingalLangParser#reassignment.
    def exitReassignment(self, ctx:AingalLangParser.ReassignmentContext):
        pass


    # Enter a parse tree produced by AingalLangParser#boolExpression.
    def enterBoolExpression(self, ctx:AingalLangParser.BoolExpressionContext):
        pass

    # Exit a parse tree produced by AingalLangParser#boolExpression.
    def exitBoolExpression(self, ctx:AingalLangParser.BoolExpressionContext):
        pass


    # Enter a parse tree produced by AingalLangParser#logicOr.
    def enterLogicOr(self, ctx:AingalLangParser.LogicOrContext):
        pass

    # Exit a parse tree produced by AingalLangParser#logicOr.
    def exitLogicOr(self, ctx:AingalLangParser.LogicOrContext):
        pass


    # Enter a parse tree produced by AingalLangParser#logicAnd.
    def enterLogicAnd(self, ctx:AingalLangParser.LogicAndContext):
        pass

    # Exit a parse tree produced by AingalLangParser#logicAnd.
    def exitLogicAnd(self, ctx:AingalLangParser.LogicAndContext):
        pass


    # Enter a parse tree produced by AingalLangParser#logicNot.
    def enterLogicNot(self, ctx:AingalLangParser.LogicNotContext):
        pass

    # Exit a parse tree produced by AingalLangParser#logicNot.
    def exitLogicNot(self, ctx:AingalLangParser.LogicNotContext):
        pass


    # Enter a parse tree produced by AingalLangParser#logicPrimaryWrap.
    def enterLogicPrimaryWrap(self, ctx:AingalLangParser.LogicPrimaryWrapContext):
        pass

    # Exit a parse tree produced by AingalLangParser#logicPrimaryWrap.
    def exitLogicPrimaryWrap(self, ctx:AingalLangParser.LogicPrimaryWrapContext):
        pass


    # Enter a parse tree produced by AingalLangParser#numComparison.
    def enterNumComparison(self, ctx:AingalLangParser.NumComparisonContext):
        pass

    # Exit a parse tree produced by AingalLangParser#numComparison.
    def exitNumComparison(self, ctx:AingalLangParser.NumComparisonContext):
        pass


    # Enter a parse tree produced by AingalLangParser#stringComparison.
    def enterStringComparison(self, ctx:AingalLangParser.StringComparisonContext):
        pass

    # Exit a parse tree produced by AingalLangParser#stringComparison.
    def exitStringComparison(self, ctx:AingalLangParser.StringComparisonContext):
        pass


    # Enter a parse tree produced by AingalLangParser#matrixComparison.
    def enterMatrixComparison(self, ctx:AingalLangParser.MatrixComparisonContext):
        pass

    # Exit a parse tree produced by AingalLangParser#matrixComparison.
    def exitMatrixComparison(self, ctx:AingalLangParser.MatrixComparisonContext):
        pass


    # Enter a parse tree produced by AingalLangParser#logicParen.
    def enterLogicParen(self, ctx:AingalLangParser.LogicParenContext):
        pass

    # Exit a parse tree produced by AingalLangParser#logicParen.
    def exitLogicParen(self, ctx:AingalLangParser.LogicParenContext):
        pass


    # Enter a parse tree produced by AingalLangParser#trueLiteral.
    def enterTrueLiteral(self, ctx:AingalLangParser.TrueLiteralContext):
        pass

    # Exit a parse tree produced by AingalLangParser#trueLiteral.
    def exitTrueLiteral(self, ctx:AingalLangParser.TrueLiteralContext):
        pass


    # Enter a parse tree produced by AingalLangParser#falseLiteral.
    def enterFalseLiteral(self, ctx:AingalLangParser.FalseLiteralContext):
        pass

    # Exit a parse tree produced by AingalLangParser#falseLiteral.
    def exitFalseLiteral(self, ctx:AingalLangParser.FalseLiteralContext):
        pass


    # Enter a parse tree produced by AingalLangParser#logicIdentifier.
    def enterLogicIdentifier(self, ctx:AingalLangParser.LogicIdentifierContext):
        pass

    # Exit a parse tree produced by AingalLangParser#logicIdentifier.
    def exitLogicIdentifier(self, ctx:AingalLangParser.LogicIdentifierContext):
        pass


    # Enter a parse tree produced by AingalLangParser#comparisonOp.
    def enterComparisonOp(self, ctx:AingalLangParser.ComparisonOpContext):
        pass

    # Exit a parse tree produced by AingalLangParser#comparisonOp.
    def exitComparisonOp(self, ctx:AingalLangParser.ComparisonOpContext):
        pass



del AingalLangParser