package lox;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static lox.TokenType.*;

class Parser {

    private class VariableTable {

        private final VariableTable enclosing;
        private final Map<String, Token> name_type = new HashMap<>();
        private final Set<String> mutables = new HashSet<>();

        VariableTable() {
            this.enclosing = null;
        }

        VariableTable(VariableTable enclosing) {
            this.enclosing = enclosing;
        }

        Token add(String name, Token type, boolean isMutable) {
            if (isMutable)
                mutables.add(name);

            return name_type.put(name, type);
        }

        boolean isMutable(String name) {
            return mutables.contains(name);
        }

        Token getType(String name) {
            if (name_type.containsKey(name)) {
                return name_type.get(name);
            }

            if (enclosing != null) 
                return enclosing.getType(name);

            return null;
        }
    }

    private VariableTable variableTable = new VariableTable();
    
    private static class ParseError extends RuntimeException {}

    private final List<Token> tokens;
    private int current = 0;

    Parser(List<Token> tokens) {
        this.tokens = tokens;
    }

    List<Stmt> parse() {
        List<Stmt> statements = new ArrayList<>();
        while (!isAtEnd() && !check(RIGHT_BRACE)) {
            // skip empty lines
            if (match(NL)) 
                continue;

            try {
                statements.add(declaration());
            }
            catch (ParseError error) {
                synchronize();
            }
        }

        return statements;
    }

    private Stmt declaration() {
        if (match(VAR))
            return varDeclaration(false);

        if (match(MUT)) {
            consume(VAR, "Expect 'var' after 'mut'.");
            return varDeclaration(true);
        }
            
        return statement();
    }

    private Stmt varDeclaration(boolean isMutable) {
        Token name = consume(IDENTIFIER, "Expect variable/constant name.");
        Token varType = null;
        if (match(COLON)) {
            varType = advance();
            if (varType.isNotTokenType(NUM_TYPE) && 
                varType.isNotTokenType(STR_TYPE) && 
                varType.isNotTokenType(BOOL_TYPE) && 
                varType.isNotTokenType(LIST_TYPE)) {
                throw error(name, "Invalid type '" + varType.lexeme + "'.");
            }
        }

        consume(EQUAL, "Expect '=' after variable/constant name.");

        Expr initializer = expression();

        if (varType == null){
            if (initializer.valueType.isTokenType(NIL)) {
                throw error(name, "Initializer cannot be nil, type unknown.");
            }
            varType = initializer.valueType;
        }
        else {
            if (varType.isNotTokenType(initializer.valueType.tokenType) && initializer.valueType.isNotTokenType(NIL)) {
                throw error(name, "Initializer type '" + initializer.valueType.lexeme + "' does not match variable type '" + varType.lexeme + "'.");
            }
        }
        
        consume(NL, "Expect a new line after variable/constant declaration.");
        variableTable.add(name.lexeme, varType, isMutable); // allow redeclaration of variables

        return new Stmt.VarDecl(name, initializer, isMutable);
    }
    
    private Stmt statement() {
        if (match(NL))
            return null;

        if (match(IF)) {
            return ifStatement();
        }

        if (match(LEFT_BRACE)) {
            return new Stmt.Block(block(new VariableTable(this.variableTable)));
        }

        if (match(PRINT)) {
            return printStatement();
        }

        // try assignment statement
        Expr expr = expression();
        if (match(EQUAL)) {
            return assignmentStatement(expr);
        }
        else {
            // not an assignment statement, return as an expression statement as default
            return expressionStatement(expr);
        }
    }

    private Stmt ifStatement() {
        Expr condition = expression();
        if (condition.valueType.isNotTokenType(BOOL_TYPE)) {
            throw error(condition.valueType, "requires bool condition.");
        }
        
        match(NL); // skip empty lines
        consume(LEFT_BRACE, "Expect '{' after 'if'.");

        Stmt thenBranch = new Stmt.Block(block(new VariableTable(this.variableTable)));
        Stmt elseBranch = null;
        if (match(ELSE)) {
            match(NL); // skip empty lines

            if (match(IF)) {
                elseBranch = ifStatement();
            }
            else {
                consume(LEFT_BRACE, "Expect '{' after 'else'.");
                elseBranch = new Stmt.Block(block(new VariableTable(this.variableTable)));
            }
        }

        return new Stmt.If(condition, thenBranch, elseBranch);
    }
    
    private List<Stmt> block(VariableTable newVariableTable) {
        VariableTable enclosing = this.variableTable;
        this.variableTable = newVariableTable;
        try {
            consume(NL, "Expect a new line after '{'.");

            List<Stmt> statements = parse();
            consume(RIGHT_BRACE, "Expect '}' after block.");
            consume(NL, "Expect a new line after '}'.");
            return statements;
        }
        finally {
            this.variableTable = enclosing;
        }
    }
    
    private Stmt printStatement() {
        Expr value = expression();
        consume(NL, "Expect a new line after the print statement.");
        return new Stmt.Print(value);
    }
    
    private Stmt assignmentStatement(Expr expr) {
        Token name = get_left_value(expr);
        Expr r_value = expression();
        
        // handle list item assignment
        if (expr instanceof Expr.Variable) { 
            Expr.Variable varExpr = (Expr.Variable)expr;
            if (varExpr.index != null) { 
                consume(NL, "Expect a new line after assignment.");
                return new Stmt.Assign(name, varExpr.index, r_value);
            }
        }

        if (valueTypeIsDifferent(expr, r_value) && r_value.valueType.isNotTokenType(NIL)) {
            throw error(name, "requires both operands to be of the same type.");
        }

        consume(NL, "Expect a new line after assignment.");
        return new Stmt.Assign(name, null, r_value);
    }

    private Token get_left_value(Expr expr) {
        if (expr instanceof Expr.Variable) {
            Expr.Variable varExpr = (Expr.Variable)expr;

            if (!variableTable.isMutable(varExpr.name.lexeme)) {
                throw error(varExpr.name, "Cannot assign to immutable variables.");
            }

            return varExpr.name;
        }
        
        if (expr instanceof Expr.Grouping)
            return get_left_value(((Expr.Grouping) expr).expression);

        // previous token is '=''
        throw error(previous(), "Invalid l-value of assignment.");
    }

    private Stmt expressionStatement(Expr expr) {
        consume(NL, "Expect a new line after expression.");
        return new Stmt.Expression(expr);
    }

    private boolean valueTypeIsDifferent(Expr exp1, Expr exp2) {
        return exp1.valueType.isNotTokenType(exp2.valueType.tokenType);
    }

    private Expr expression() {
        return comma();
    }

    private Expr comma() {
        // no left operand error production
        if (match(COMMA)) {
            Token operator = previous();
            comma(); // Discard the right-hand side of the comma operator

            throw error(operator, "Expect left expression.");
        }

        Expr expr = ternaryConditional();

        while (match(COMMA)) {
            Token operator = previous();
            while (match(NL)); // skip empty lines
            Expr right = ternaryConditional();

            if (valueTypeIsDifferent(expr, right)) {
                throw error(operator, "requires both operands to be of the same type.");
            }

            expr = new Expr.Binary(expr, operator, right, expr.valueType); 
        }

        return expr;
    }

    private Expr ternaryConditional() {
        Expr expr = equality();

        if (match(QUESTION)) {
            Token operator = previous();
            while (match(NL)); // skip empty lines
            
            if (expr.valueType.isNotTokenType(BOOL_TYPE)) {
                throw error(operator, "requires bool condition.");
            }

            Expr thenBranch = ternaryConditional();
            consume(COLON, "Expect ':' after then branch");
            while (match(NL)); // skip empty lines
            Expr elseBranch = ternaryConditional();

            if (valueTypeIsDifferent(thenBranch, elseBranch)) {
                throw error(operator, "requires both branches to be of the same type.");
            }

            expr = new Expr.TernaryConditional(expr, operator, thenBranch, elseBranch, thenBranch.valueType); 
        }

        return expr;
    }

    private Expr equality() {
        // no left operand error production
        if (match(BANG_EQUAL, EQUAL_EQUAL)) {
            Token operator = previous();
            equality(); // Discard the right-hand side of the equality operator

            throw error(operator, "Expect left expression.");
        }

        Expr expr = comparison();

        while (match(BANG_EQUAL, EQUAL_EQUAL)) {
            Token operator = previous();
            Expr right = comparison();

            if (valueTypeIsDifferent(expr, right)) {
                throw error(operator, "requires both operands to be of the same type.");
            }
            if (expr.valueType.isNotTokenType(NUM_TYPE) && expr.valueType.isNotTokenType(BOOL_TYPE)) {
                throw error(operator, "does not support " + expr.valueType.lexeme);
            }

            expr = new Expr.Binary(expr, operator, right, new Token(TokenType.BOOL_TYPE, "bool", null, operator.line));
        }

        return expr;
    }

    private Expr comparison() {
        // no left operand error production
        if (match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
            Token operator = previous();
            comparison(); // Discard the right-hand side of the comparison operator

            throw error(operator, "Expect left expression.");
        }

        Expr expr = term();

        while (match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
            Token operator = previous();
            Expr right = term();

            if (valueTypeIsDifferent(expr, right)) {
                throw error(operator, "requires both operands to be of the same type.");
            }
            if (expr.valueType.isNotTokenType(NUM_TYPE) && expr.valueType.isNotTokenType(STR_TYPE)) {
                throw error(operator, "does not support " + expr.valueType.lexeme);
            }

            expr = new Expr.Binary(expr, operator, right, new Token(TokenType.BOOL_TYPE, "bool", null, operator.line));
        }

        return expr;
    }

    private Expr term() {
        // no left operand error production
        if (match(PLUS)) {
            Token operator = previous();
            term(); // Discard the right-hand side of the term operator

            throw error(operator, "Expect left expression.");
        }

        Expr expr = factor();

        while (match(MINUS, PLUS)) {
            Token operator = previous();
            Expr right = factor();

            if (valueTypeIsDifferent(expr, right)) {
                throw error(operator, "requires both operands to be of the same type.");
            }
            if (operator.isTokenType(PLUS))
                if (expr.valueType.isNotTokenType(NUM_TYPE) && 
                    expr.valueType.isNotTokenType(STR_TYPE) &&
                    expr.valueType.isNotTokenType(LIST_TYPE)) {
                    throw error(operator, "does not support " + expr.valueType.lexeme);
                }
            if (operator.isTokenType(MINUS) && expr.valueType.isNotTokenType(NUM_TYPE)) {
                throw error(operator, "does not support " + expr.valueType.lexeme);
            }
            
            expr = new Expr.Binary(expr, operator, right, expr.valueType);
        }

        return expr;
    }

    private Expr factor() {
        // no left operand error production
        if (match(SLASH, STAR)) {
            Token operator = previous();
            factor(); // Discard the right-hand side of the factor operator

            throw error(operator, "Expect left expression.");
        }

        Expr expr = unary();

        while (match(SLASH, STAR)) {
            Token operator = previous();
            Expr right = unary();

            
            if (right.valueType.isNotTokenType(NUM_TYPE)){
                throw error(operator, "does not support " + expr.valueType.lexeme + ", " + right.valueType.lexeme);
            }
            if (operator.isTokenType(STAR))
                if (expr.valueType.isNotTokenType(LIST_TYPE) && 
                    expr.valueType.isNotTokenType(NUM_TYPE) &&
                    expr.valueType.isNotTokenType(STR_TYPE)) {
                    throw error(operator, "does not support " + expr.valueType.lexeme + ", " + right.valueType.lexeme);
                }
            if (operator.isTokenType(SLASH))
                if (expr.valueType.isNotTokenType(NUM_TYPE)) {
                    throw error(operator, "does not support " + expr.valueType.lexeme + ", " + right.valueType.lexeme);
                }

            expr = new Expr.Binary(expr, operator, right, expr.valueType);
        }

        return expr;
    }

    private Expr unary() {
        if (match(BANG, MINUS)) {
            Token operator = previous();
            Expr right = unary();

            if (operator.isTokenType(MINUS) && right.valueType.isNotTokenType(NUM_TYPE)) {
                throw error(operator, "does not support " + right.valueType.lexeme);
            }
            if (operator.isTokenType(BANG) && right.valueType.isNotTokenType(BOOL_TYPE)) {
                throw error(operator, "does not support " + right.valueType.lexeme);
            }

            return new Expr.Unary(operator, right, right.valueType);
        }

        return primary();
    }

    private Expr primary() {
        if (match(FALSE)) {
            return new Expr.Literal(false, new Token(BOOL_TYPE, "bool", null, previous().line));
        }

        if (match(TRUE)) {
            return new Expr.Literal(true, new Token(BOOL_TYPE, "bool", null, previous().line));
        }

        if (match(NIL)) {
            return new Expr.Literal(null, new Token(NIL, "nil", null, previous().line));
        }

        if (match(STR_LITERAL)) {
            return new Expr.Literal(previous().literal, new Token(STR_TYPE, "str", null, previous().line));
        }

        if (match(NUM_LITERAL)) {
            return new Expr.Literal(previous().literal, new Token(NUM_TYPE, "num", null, previous().line));
        }

        if (match(LEFT_PAREN)) {
            Expr expr = expression();
            consume(RIGHT_PAREN, "Expect ')' after expression.");
            return new Expr.Grouping(expr, expr.valueType);
        }

        if (match(LEFT_SQUARE)) {
            List<Expr> items = new ArrayList<>();
            if (!check(RIGHT_SQUARE)) {
                do {
                    // comma expression is not allowed
                    items.add(ternaryConditional());
                } while (match(COMMA));
            }
            consume(RIGHT_SQUARE, "Expect ']' after list.");
            return new Expr.List_(items, new Token(LIST_TYPE, "list", null, previous().line));
        }

        if (match(IDENTIFIER)) {
            Token id = previous();
            Token id_type = variableTable.getType(id.lexeme);
            if (id_type == null) {
                throw error(previous(), "Undefined variable '" + previous().lexeme + "'.");
            }

            Expr index = null;
            if (id_type.isTokenType(LIST_TYPE)) {
                consume(LEFT_SQUARE, "Expect '[' after list variable.");
                index = expression();
                if (index.valueType.isNotTokenType(NUM_TYPE)) {
                    throw error(id, "List only supports integer index.");
                }
                consume(RIGHT_SQUARE, "Expect ']' after list variable.");
            }

            return new Expr.Variable(id, index, id_type);
        }

        throw error(peek(), "Expect expression.");
    }

    private boolean match(TokenType... types) {
        for (TokenType type : types) {
            if (check(type)) {
                advance();
                return true;
            }
        }

        return false;
    }

    private Token consume(TokenType type, String message) {
        if (check(type)) {
            return advance();
        }

        throw error(peek(), message);
    }

    private ParseError error(Token token, String message) {
        Lox.error(token, message);
        return new ParseError();
    }

    private void synchronize() {
        while (!isAtEnd()) {
            switch (peek().tokenType) {
                case NL:
                    advance();
                    return;
                case CLASS:
                case FUN:
                case MUT:
                case VAR:
                case FOR:
                case IF:
                case WHILE:
                case PRINT:
                case RETURN:
                case LEFT_BRACE:
                case RIGHT_BRACE:
                    return;
                default: 
                    // Do nothing, just keep looking for a statement
                    break;
            }

            advance();
        }
    }
    
    private boolean check(TokenType type) {
        if (isAtEnd()) {
            return false;
        }

        return peek().isTokenType(type);
    }

    private Token advance() {
        if (!isAtEnd()) {
            current++;
        }

        return previous();
    }

    private boolean isAtEnd() {
        return peek().isTokenType(EOF);
    }

    private Token peek() {
        return tokens.get(current);
    }

    private Token previous() {
        return tokens.get(current - 1);
    }
}
