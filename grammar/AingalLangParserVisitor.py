# Generated from AingalLangParser.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .AingalLangParser import AingalLangParser
else:
    from AingalLangParser import AingalLangParser

# This class defines a complete generic visitor for a parse tree produced by AingalLangParser.

class AingalLangParserVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by AingalLangParser#program.
    def visitProgram(self, ctx:AingalLangParser.ProgramContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#statement.
    def visitStatement(self, ctx:AingalLangParser.StatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#loopStatements.
    def visitLoopStatements(self, ctx:AingalLangParser.LoopStatementsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#variableDeclarationOrAssignment.
    def visitVariableDeclarationOrAssignment(self, ctx:AingalLangParser.VariableDeclarationOrAssignmentContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#matrixExpression.
    def visitMatrixExpression(self, ctx:AingalLangParser.MatrixExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#matrixAtom.
    def visitMatrixAtom(self, ctx:AingalLangParser.MatrixAtomContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#matrixConstruction.
    def visitMatrixConstruction(self, ctx:AingalLangParser.MatrixConstructionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#row.
    def visitRow(self, ctx:AingalLangParser.RowContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#value.
    def visitValue(self, ctx:AingalLangParser.ValueContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#stringExpression.
    def visitStringExpression(self, ctx:AingalLangParser.StringExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#expression.
    def visitExpression(self, ctx:AingalLangParser.ExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#scopedIdentifier.
    def visitScopedIdentifier(self, ctx:AingalLangParser.ScopedIdentifierContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#typeAnnotation.
    def visitTypeAnnotation(self, ctx:AingalLangParser.TypeAnnotationContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#functionDeclaration.
    def visitFunctionDeclaration(self, ctx:AingalLangParser.FunctionDeclarationContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#parameter.
    def visitParameter(self, ctx:AingalLangParser.ParameterContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#typedParameter.
    def visitTypedParameter(self, ctx:AingalLangParser.TypedParameterContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#returnStatement.
    def visitReturnStatement(self, ctx:AingalLangParser.ReturnStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#functionCall.
    def visitFunctionCall(self, ctx:AingalLangParser.FunctionCallContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#argumentList.
    def visitArgumentList(self, ctx:AingalLangParser.ArgumentListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#builtInFunctions.
    def visitBuiltInFunctions(self, ctx:AingalLangParser.BuiltInFunctionsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#ifStatement.
    def visitIfStatement(self, ctx:AingalLangParser.IfStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#loopIfStatement.
    def visitLoopIfStatement(self, ctx:AingalLangParser.LoopIfStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#loopStatement.
    def visitLoopStatement(self, ctx:AingalLangParser.LoopStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#forLoop.
    def visitForLoop(self, ctx:AingalLangParser.ForLoopContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#forInit.
    def visitForInit(self, ctx:AingalLangParser.ForInitContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#forUpdate.
    def visitForUpdate(self, ctx:AingalLangParser.ForUpdateContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#forBody.
    def visitForBody(self, ctx:AingalLangParser.ForBodyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#whileLoop.
    def visitWhileLoop(self, ctx:AingalLangParser.WhileLoopContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#displayStatement.
    def visitDisplayStatement(self, ctx:AingalLangParser.DisplayStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#blockStatement.
    def visitBlockStatement(self, ctx:AingalLangParser.BlockStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#numExpression.
    def visitNumExpression(self, ctx:AingalLangParser.NumExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#term.
    def visitTerm(self, ctx:AingalLangParser.TermContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#unaryPlus.
    def visitUnaryPlus(self, ctx:AingalLangParser.UnaryPlusContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#unaryMinus.
    def visitUnaryMinus(self, ctx:AingalLangParser.UnaryMinusContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#factorFunctionCall.
    def visitFactorFunctionCall(self, ctx:AingalLangParser.FactorFunctionCallContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#factorNumber.
    def visitFactorNumber(self, ctx:AingalLangParser.FactorNumberContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#factorscopedIdentifier.
    def visitFactorscopedIdentifier(self, ctx:AingalLangParser.FactorscopedIdentifierContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#factorIdentifier.
    def visitFactorIdentifier(self, ctx:AingalLangParser.FactorIdentifierContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#factorString.
    def visitFactorString(self, ctx:AingalLangParser.FactorStringContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#factorOperation.
    def visitFactorOperation(self, ctx:AingalLangParser.FactorOperationContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#factorParens.
    def visitFactorParens(self, ctx:AingalLangParser.FactorParensContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#castExpression.
    def visitCastExpression(self, ctx:AingalLangParser.CastExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#factorTrue.
    def visitFactorTrue(self, ctx:AingalLangParser.FactorTrueContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#factorFalse.
    def visitFactorFalse(self, ctx:AingalLangParser.FactorFalseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#operation.
    def visitOperation(self, ctx:AingalLangParser.OperationContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#leftHandSide.
    def visitLeftHandSide(self, ctx:AingalLangParser.LeftHandSideContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#reassignment.
    def visitReassignment(self, ctx:AingalLangParser.ReassignmentContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#boolExpression.
    def visitBoolExpression(self, ctx:AingalLangParser.BoolExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#logicOr.
    def visitLogicOr(self, ctx:AingalLangParser.LogicOrContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#logicAnd.
    def visitLogicAnd(self, ctx:AingalLangParser.LogicAndContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#logicNot.
    def visitLogicNot(self, ctx:AingalLangParser.LogicNotContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#logicPrimaryWrap.
    def visitLogicPrimaryWrap(self, ctx:AingalLangParser.LogicPrimaryWrapContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#numComparison.
    def visitNumComparison(self, ctx:AingalLangParser.NumComparisonContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#stringComparison.
    def visitStringComparison(self, ctx:AingalLangParser.StringComparisonContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#matrixComparison.
    def visitMatrixComparison(self, ctx:AingalLangParser.MatrixComparisonContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#logicParen.
    def visitLogicParen(self, ctx:AingalLangParser.LogicParenContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#trueLiteral.
    def visitTrueLiteral(self, ctx:AingalLangParser.TrueLiteralContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#falseLiteral.
    def visitFalseLiteral(self, ctx:AingalLangParser.FalseLiteralContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#logicIdentifier.
    def visitLogicIdentifier(self, ctx:AingalLangParser.LogicIdentifierContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AingalLangParser#comparisonOp.
    def visitComparisonOp(self, ctx:AingalLangParser.ComparisonOpContext):
        return self.visitChildren(ctx)



del AingalLangParser