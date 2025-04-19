package lox;

import static lox.TokenType.BANG;
import static lox.TokenType.BOOL_TYPE;
import static lox.TokenType.MINUS;
import static lox.TokenType.NUM_TYPE;
import static lox.TokenType.STR_TYPE;

import lox.Parser.ParseError;

class ExprStaticChecker implements Expr.Visitor<ParseError> {
    
    public static class DifferentTypeError extends ParseError {
        public DifferentTypeError(String typeLexeme) {
            super("Left and right expressions of '" + typeLexeme + "' must be of the same type.");
        }
    }

    public static class TypeError extends ParseError {
        public TypeError(String message) {
            super(message);
        }
    }

    public ParseError check(Expr expr) {
        return expr.accept(this);
    }

    private boolean isSameValueType(Expr left, Expr right) {
        return left.valueType == right.valueType;
    }

    @Override
    public ParseError visit(Expr.TernaryConditional expr) {
        if (expr.condition.valueType != BOOL_TYPE)
            return new TypeError("Condition of ?: must be boolean.");
        if (!isSameValueType(expr.thenBranch, expr.elseBranch))
            return new DifferentTypeError(expr.question.lexeme);
        
        return null;
    }

    @Override
    public ParseError visit(Expr.Binary expr) {
        switch (expr.operator.tokenType) {
            case COMMA:
                if (!isSameValueType(expr.left, expr.right))
                    return new DifferentTypeError(expr.operator.lexeme);
                break;
            case OR: case AND:
                if (expr.left.valueType != TokenType.BOOL_TYPE)
                    return new TypeError("Left expression is not boolean.");
                if (expr.right.valueType != TokenType.BOOL_TYPE)
                    return new TypeError("Right expression is not boolean.");
                break;
            case EQUAL_EQUAL: case BANG_EQUAL:
                if (!isSameValueType(expr.left, expr.right))
                    return new DifferentTypeError(expr.operator.lexeme);
                if ((expr.left.valueType != NUM_TYPE) && (expr.left.valueType != STR_TYPE))
                    return new TypeError(expr.operator.lexeme + " only support 'num' and 'str' types.");
                break;
            case GREATER: case GREATER_EQUAL: case LESS: case LESS_EQUAL:
                if (!isSameValueType(expr.left, expr.right))
                    return new DifferentTypeError(expr.operator.lexeme);
                if ((expr.left.valueType != NUM_TYPE) && (expr.left.valueType != STR_TYPE))
                    return new TypeError(expr.operator.lexeme + " only support 'num' and 'str' types.");
                break;
            case MINUS:
                if (!isSameValueType(expr.left, expr.right))
                    return new DifferentTypeError(expr.operator.lexeme);
                if (expr.left.valueType != NUM_TYPE)
                    return new TypeError(expr.operator.lexeme + " only support 'num' and 'str' types.");
                break;
            case PLUS:
                if (!isSameValueType(expr.left, expr.right))
                    return new DifferentTypeError(expr.operator.lexeme);
                if ((expr.left.valueType != NUM_TYPE) && (expr.left.valueType != STR_TYPE))
                    return new TypeError(expr.operator.lexeme + " only support 'num' and 'str' types.");
                break;
            case STAR: case SLASH:
                if (!isSameValueType(expr.left, expr.right))
                    return new DifferentTypeError(expr.operator.lexeme);
                if (expr.left.valueType != NUM_TYPE)
                    return new TypeError(expr.operator.lexeme + " only support 'num' type.");
                break;
            default:
                throw new RuntimeException("Unknown operator: " + expr.operator.tokenType);
        }

        return null;
    }

    @Override
    public ParseError visit(Expr.Unary expr) {
        if ((expr.operator.tokenType == MINUS) && (expr.right.valueType != NUM_TYPE)) {
            return new TypeError("Unary '-' only support 'num' type.");
        }
        if ((expr.operator.tokenType == BANG) && (expr.right.valueType != BOOL_TYPE)) {
            return new TypeError("'!' only support 'bool' type.");
        }

        return null;
    }

    @Override
    public ParseError visit(Expr.Call expr) {
        if (expr.callee.valueType != TokenType.CALLABLE) {
            return new TypeError("Only functions and classes can be called.");
        }

        return null;
    }

    @Override
    public ParseError visit(Expr.Grouping expr) {
        return null;
    }

    @Override
    public ParseError visit(Expr.Literal expr) {
        return null;
    }

    @Override
    public ParseError visit(Expr.List_ expr) {
        return null;
    }

    @Override
    public ParseError visit(Expr.Variable expr) {
        return null;
    }

    @Override
    public ParseError visit(Expr.Lambda expr) {
        return null;
    }
}
