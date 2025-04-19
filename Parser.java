package lox;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static lox.TokenType.*;

class Parser {

    public static class ParseError extends RuntimeException {
        public ParseError() {}
        public ParseError(String message) {
            super(message);
        }
    }

    private class VariableTable {

        private final VariableTable enclosing;
        private final Map<String, TokenType> name_type = new HashMap<>();
        private final Set<String> mutables = new HashSet<>();

        VariableTable() {
            this.enclosing = null;
        }

        VariableTable(VariableTable enclosing) {
            this.enclosing = enclosing;
        }

        TokenType add(String name, TokenType type, boolean isMutable) {
            if (isMutable)
                mutables.add(name);

            return name_type.put(name, type);
        }

        boolean isMutable(String name) {
            if (mutables.contains(name))
                return true;
            
            if (enclosing != null)
                return enclosing.isMutable(name);
            
            return false;
        }

        TokenType getType(String name) {
            if (name_type.containsKey(name)) {
                return name_type.get(name);
            }

            if (enclosing != null) 
                return enclosing.getType(name);

            return null;
        }
    }

    private boolean hasLoopTermination = false;
    private VariableTable variableTable = new VariableTable();
    private final ExprStaticChecker staticChecker = new ExprStaticChecker();
    private final List<Token> tokens;
    private int current = 0;
    private TokenType EOS = TokenType.NL;

    Parser(List<Token> tokens) {
        this.tokens = tokens;
    }

    List<Stmt> parse() {
        List<Stmt> statements = new ArrayList<>();
        while (!isAtEnd() && !check(RIGHT_BRACE)) {
            // skip empty lines
            if (match(EOS)) 
                continue;

            try {
                statements.add(declaration());

                if (hasLoopTermination) { // put remaining statements in a if block testing the break variable
                    hasLoopTermination = false;

                    current--; // recover the last EOS token for the next then block
                    Stmt.Block thenBranch = new Stmt.Block(block(new VariableTable(this.variableTable)));
                    current -= 2; // recover the last } EOS tokens for the current block

                    if (!thenBranch.statements.isEmpty()) {
                        Token name = new Token(TokenType.IDENTIFIER, "break", null, previous().line);
                        Expr.Variable breakVari = new Expr.Variable(name, BOOL_TYPE);
                        Expr.Unary notBreak = new Expr.Unary(
                            new Token(BANG, "!", null, previous().line), 
                            breakVari, 
                            BOOL_TYPE
                        );
                        Stmt.If ifStmt = new Stmt.If(notBreak, thenBranch, null);
                        statements.add(ifStmt);
                    }

                    hasLoopTermination = true;
                    break;
                }
            }
            catch (ParseError error) {
                synchronize();
            }
        }

        return statements;
    }

    private Stmt declaration() {
        if (match(FUN) && check(IDENTIFIER) ) {
            return function("function");
        }

        if (match(VAR)) {
            return varDeclaration(match(MUT));
        }

        return statement();
    }

    private Stmt.Function function(String kind) {
        Token funName = consume(IDENTIFIER, "Expect " + kind + " name.");
        match(EOS);

        // bind lambda to the function name
        Expr.Lambda lambda = lambda();
        this.variableTable.add(funName.lexeme, CALLABLE, false); // allow redeclaration of functions
        return new Stmt.Function(funName, lambda);
    }

    private Stmt varDeclaration(boolean isMutable) {
        Token name = consume(IDENTIFIER, "Expect variable/constant name.");
        TokenType varType = null;
        if (match(COLON)) {
            if (match(NUM_TYPE, STR_TYPE, BOOL_TYPE)) {
                varType = previous().tokenType;
            }
            else {
                throw error(peek(), "Invalid type '" + peek().lexeme + "'.");
            }
        }

        consume(EQUAL, "Expect '=' after variable/constant name.");

        Expr initializer = expression();

        if (varType == null){
            if (initializer.valueType == NIL) {
                throw error(name, "Initializer cannot be nil, type unknown.");
            }
            varType = initializer.valueType;
        }
        else {
            if ((varType != initializer.valueType) && (initializer.valueType != NIL)) {
                error(name, "Initializer type '" + initializer.valueType.name() + "' does not match variable type '" + varType.name() + "'.");
            }
        }
        
        consume(EOS, "Expect '" + EOS + "' after variable/constant declaration.");
        
        variableTable.add(name.lexeme, varType, isMutable); // allow redeclaration of variables
        return new Stmt.VarDecl(name, initializer, isMutable);
    }
    
    private Stmt statement() {

        if (match(IF)) {
            return ifStatement();
        }

        if (match(LEFT_BRACE)) {
            return new Stmt.Block(block(new VariableTable(this.variableTable)));
        }

        if (match(PRINT)) {
            return printStatement();
        }

        if (match(RETURN)) {
            return returnStatement();
        }

        if (match(WHILE)) {
            return whileStatement();
        }

        if (match(BREAK)) {
            return breakStatement(false);
        }

        if (match(CONTINUE)) {
            return breakStatement(true);
        }

        if (match(FOR)) {
            return forStatement();
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
        if (condition.valueType != BOOL_TYPE) {
            error(previous(), "Requires bool condition.");
        }
        
        match(EOS); // skip empty lines
        consume(LEFT_BRACE, "Expect '{' after condition.");

        Stmt thenBranch = new Stmt.Block(block(new VariableTable(this.variableTable)));
        Stmt elseBranch = null;
        if (match(ELSE)) {
            match(EOS); // skip empty lines

            if (match(IF)) { // else if clause
                elseBranch = ifStatement();
            }
            else {
                consume(LEFT_BRACE, "Expect '{' after 'else'.");
                elseBranch = new Stmt.Block(block(new VariableTable(this.variableTable)));
            }
        }

        return new Stmt.If(condition, thenBranch, elseBranch);
    }
    
    private List<Stmt> block(VariableTable newVarTable) {
        this.variableTable = newVarTable;
        try {
            consume(EOS, "Expect '" + EOS + "' after '{'.");

            List<Stmt> statements = parse();
            consume(RIGHT_BRACE, "Expect '}' after block.");
            consume(EOS, "Expect '" + EOS + "' after '}'.");
            return statements;
        }
        finally {
            this.variableTable = newVarTable.enclosing;
        }
    }
    
    private Stmt printStatement() {
        Expr value = expression();
        consume(EOS, "Expect '" + EOS + "' after the print statement.");
        return new Stmt.Print(value);
    }
    
    private Stmt returnStatement() {
        Token keyword = previous();
        if (match(EOS)) {
            this.variableTable.add("return", NIL, false); // used to check type after return
            return new Stmt.Return(keyword, null);
        }
        Expr value = expression();
        consume(EOS, "Expect '" + EOS + "' after return statement.");
        this.variableTable.add("return", value.valueType, false);

        return new Stmt.Return(keyword, value);
    }

    private Stmt breakStatement(boolean isContinue) {
        consume(EOS, "Expect '" + EOS + "' after " + previous().lexeme + " statement.");
        // skip following statements
        while (!check(RIGHT_BRACE)) {
            advance();
        }

        // check if break/continue is inside a loop
        if (variableTable.getType("break") == null) {
            error(previous(), "Not inside a loop.");
        }

        hasLoopTermination = true;
        // change break/continue variable to true
        Stmt setBreak = new Stmt.Assign(
            new Token(TokenType.IDENTIFIER, "break", null, previous().line), 
            new Expr.Literal(true, BOOL_TYPE)
        );
        
        if (isContinue) {
            Stmt setContinue = new Stmt.Assign(
                new Token(TokenType.IDENTIFIER, "continue", null, previous().line),  
                new Expr.Literal(true, BOOL_TYPE)
            );

            return new Stmt.Block(List.of(setBreak, setContinue));
        }
        
        return new Stmt.Block(List.of(setBreak));
    }

    private Stmt whileStatement() {
        Expr condition = expression();
        if (condition.valueType != BOOL_TYPE) {
            error(previous(), "Requires bool condition.");
        }
        
        match(EOS); // skip empty lines
        consume(LEFT_BRACE, "Expect '{' after 'while'.");

        // create special 'continue' and 'break' variables
        VariableTable newVarTable = new VariableTable(this.variableTable);
        newVarTable.add("continue", BOOL_TYPE, true);
        newVarTable.add("break", BOOL_TYPE, true);
        
        Stmt body = new Stmt.Block(block(newVarTable));

        hasLoopTermination = false; // reset at the end of each loop
        return new Stmt.While(condition, body, null);
    }
    
    private Stmt forStatement() {
        // for statement has its own scope
        VariableTable enclosing = this.variableTable;
        VariableTable newVariableTable = new VariableTable(enclosing);
        this.variableTable = newVariableTable;
        this.EOS = TokenType.SEMICOLON; // allow ';' as EOS in for initializer

        try {
            Stmt initializer = null;
            if (match(EOS)) {
                initializer = null;
            }
            else if (match(VAR)) {
                consume(MUT, "Expect 'mut' after 'var' in loop initializer.");
                initializer = varDeclaration(true);
            }
            else {
                Expr targetExpr = expression();
                initializer = assignmentStatement(targetExpr);
            }

            match(NL); // skip empty lines

            Expr condition = null;
            if (!check(EOS)) {
                condition = expression();
                if (condition.valueType != BOOL_TYPE) {
                    error(previous(), "Requires bool condition.");
                }
            }
            consume(EOS, "Expect ';' after loop condition.");

            match(NL); // skip empty lines

            Stmt increment = null;
            if (!check(EOS)) {
                increment = statement();
            }

            match(NL); // skip empty lines

            consume(LEFT_BRACE, "Expect '{' after increment.");

            this.EOS = TokenType.NL; // reset EOS to NL
            
            // create special 'continue' and 'break' variables
            VariableTable newVarTable = new VariableTable(this.variableTable);
            newVarTable.add("continue", BOOL_TYPE, true);
            newVarTable.add("break", BOOL_TYPE, true);

            Stmt body = new Stmt.Block(block(newVarTable));

            if (condition == null) 
                condition = new Expr.Literal(true, BOOL_TYPE);
            body = new Stmt.While(condition, body, increment);

            if (initializer != null) {
                body = new Stmt.Block(
                    List.of(
                        initializer, 
                        body
                    )
                );
            }

            return body;
        }
        finally {
            this.variableTable = enclosing;
            hasLoopTermination = false; // reset at the end of each loop
            this.EOS = TokenType.NL; // reset EOS to NL
        }
    }
    
    private Stmt assignmentStatement(Expr targetExpr) {
        Token targetName = get_left_value(targetExpr);
        Expr r_value = expression();

        if ((targetExpr.valueType != r_value.valueType) && r_value.valueType != NIL) {
            error(targetName, "Requires both operands to be of the same type.");
        }

        consume(EOS, "Expect '" + EOS + "' after assignment.");
        return new Stmt.Assign(targetName, r_value);
    }

    private Token get_left_value(Expr expr) {
        if (expr instanceof Expr.Variable) {
            Expr.Variable varExpr = (Expr.Variable)expr;

            if (!variableTable.isMutable(varExpr.name.lexeme)) {
                error(varExpr.name, "Cannot assign to immutable variables.");
            }

            return varExpr.name;
        }
        
        if (expr instanceof Expr.Grouping)
            return get_left_value(((Expr.Grouping) expr).expression);

        // previous token is '=''
        throw error(previous(), "Invalid l-value of assignment.");
    }

    private Stmt expressionStatement(Expr expr) {
        consume(EOS, "Expect '" + EOS + "' after expression.");
        return new Stmt.Expression(expr);
    }

    private Expr.Lambda lambda() {
        consume(LEFT_PAREN, "Expect '(' after 'fun'.");
        List<Token> paramTokens = new ArrayList<>();
        VariableTable functionScope = new VariableTable(this.variableTable);
        do {
            match(EOS); // skip empty lines
            if (check(RIGHT_PAREN)) break;
            if (paramTokens.size() >= 255) {
                error(peek(), "Cannot have more than 255 parameters.");
            }
            
            boolean isMutable = match(MUT);
            Token paramName = consume(IDENTIFIER, "Expect parameter name.");
            consume(COLON, "Expect ':' after parameter name.");

            Token paramType = null;
            if (match(NUM_TYPE, STR_TYPE, BOOL_TYPE, CALLABLE)) {
                paramType = previous();
            }
            else {
                throw error(peek(), "Invalid type '" + peek().lexeme + "'.");
            }
            paramTokens.add(paramName);
            functionScope.add(paramName.lexeme, paramType.tokenType, isMutable);
        } while (match(COMMA));

        match(EOS); // skip empty lines
        consume(RIGHT_PAREN, "Expect ')' after parameters.");

        TokenType expectedRetType = NIL;
        if (match(COLON)) {
            if (match(NUM_TYPE, STR_TYPE, BOOL_TYPE, CALLABLE)) {
                expectedRetType = previous().tokenType;
            }
            else {
                throw error(peek(), "Invalid type '" + peek().lexeme + "'.");
            }
        }
        match(EOS); // skip empty lines

        consume(LEFT_BRACE, "Expect '{'");

        List<Stmt> body = block(functionScope);

        return new Expr.Lambda(paramTokens, body, expectedRetType);
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
            match(EOS); // skip empty lines
            Expr right = ternaryConditional();
            expr = new Expr.Binary(expr, operator, right, expr.valueType); 
            
            ParseError err = staticChecker.check(expr);
            if (err != null) {
                assert err instanceof ExprStaticChecker.DifferentTypeError;
                // if got wrong type, expr value type will be invalid, panic
                throw error(operator, err.getMessage());
            }
        }

        return expr;
    }

    private Expr ternaryConditional() {
        Expr expr = or();

        if (match(QUESTION)) {
            Token operator = previous();
            match(EOS); // skip empty lines
            Expr thenBranch = ternaryConditional();
            consume(COLON, "Expect ':' after then branch");
            match(EOS); // skip empty lines
            Expr elseBranch = ternaryConditional();
            expr = new Expr.TernaryConditional(expr, operator, thenBranch, elseBranch, thenBranch.valueType);
            
            ParseError err = staticChecker.check(expr);
            if (err != null) {
                if (err instanceof ExprStaticChecker.TypeError) {
                    // don't panic for condition expr type error, still in a valid state
                    error(operator, err.getMessage());
                }
                else if (err instanceof ExprStaticChecker.DifferentTypeError) {
                    // if got wrong type, expr value type will be invalid, panic
                    throw error(operator, err.getMessage());
                }
                else {
                    throw new RuntimeException("Unknown checker error type: " + err.getClass().getName());
                }
            }
        }

        return expr;
    }

    private Expr or() {
        Expr expr = and();

        while (match(OR)) {
            Token operator = previous();
            Expr right = and();
            expr = new Expr.Binary(expr, operator, right, BOOL_TYPE);

            ParseError err = staticChecker.check(expr);
            if (err != null) {
                assert err instanceof ExprStaticChecker.TypeError;
                // don't panic for type error, still in a valid state, always bool type
                error(operator, err.getMessage());
            }
        }

        return expr;
    }

    private Expr and() {
        Expr expr = equality();

        while (match(AND)) {
            Token operator = previous();
            Expr right = equality();
            expr = new Expr.Binary(expr, operator, right, BOOL_TYPE);
        
            ParseError err = staticChecker.check(expr);
            if (err != null) {
                assert err instanceof ExprStaticChecker.TypeError;
                // don't panic for type error, still in a valid state, always bool type
                error(operator, err.getMessage());
            } 
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
            expr = new Expr.Binary(expr, operator, right, BOOL_TYPE);
        
            ParseError err = staticChecker.check(expr);
            if (err != null) {
                assert (err instanceof ExprStaticChecker.DifferentTypeError) ||
                       (err instanceof ExprStaticChecker.TypeError);
                // don't panic, still in a valid state, always bool type
                error(operator, err.getMessage()); 
            }
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
            expr = new Expr.Binary(expr, operator, right, BOOL_TYPE);
        
            ParseError err = staticChecker.check(expr);
            if (err != null) {
                assert (err instanceof ExprStaticChecker.DifferentTypeError) ||
                       (err instanceof ExprStaticChecker.TypeError);
                // don't panic, still in a valid state, always bool type
                error(operator, err.getMessage()); 
            }
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
            expr = new Expr.Binary(expr, operator, right, expr.valueType);

            ParseError err = staticChecker.check(expr);
            if (err != null) {
                assert (err instanceof ExprStaticChecker.DifferentTypeError) ||
                       (err instanceof ExprStaticChecker.TypeError);
                // if got wrong type, expr value type will be invalid, panic
                throw error(operator, err.getMessage()); 
            }
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
            expr = new Expr.Binary(expr, operator, right, NUM_TYPE);
        
            ParseError err = staticChecker.check(expr);
            if (err != null) {
                assert (err instanceof ExprStaticChecker.DifferentTypeError) ||
                       (err instanceof ExprStaticChecker.TypeError);
                // don't panic, still in a valid state, always num type
                error(operator, err.getMessage()); 
            }
        }

        return expr;
    }

    private Expr unary() {
        if (match(BANG, MINUS)) {
            Token operator = previous();
            Expr right = unary();
            
            Expr.Unary expr = null;
            if (operator.tokenType == MINUS) {
                expr = new Expr.Unary(operator, right, NUM_TYPE);
            }
            else if (operator.tokenType == BANG) {
                expr = new Expr.Unary(operator, right, BOOL_TYPE);
            }
            else {
                throw new RuntimeException("Invalid unary operator.");
            }
                
        
            ParseError err = staticChecker.check(expr);
            if (err != null) {
                assert err instanceof ExprStaticChecker.TypeError;
                // don't panic, still in a valid state
                error(operator, err.getMessage()); 
            }

            return expr;
        }

        return call();
    }

    private Expr call() {
        Expr expr = primary();

        while (true) {
            if (match(LEFT_PAREN)) {
                expr = finishCall(expr);
            }
            else {
                break;
            }
        }

        return expr;
    }

    private Expr finishCall(Expr expr) {
        List<Expr> arguments = new ArrayList<>();
        Token paren = null;

        do {
            match(EOS); // skip empty lines
            if (check(RIGHT_PAREN)) break;

            Expr argument = ternaryConditional(); // comma expression is not allowed
            arguments.add(argument);
        } while (match(COMMA));

        paren = consume(RIGHT_PAREN, "Expect ')' after arguments.");
        return new Expr.Call(expr, paren, arguments, CALLABLE); // temp workaround
    }

    private Expr primary() {
        if (match(FALSE)) {
            return new Expr.Literal(false, BOOL_TYPE);
        }

        if (match(TRUE)) {
            return new Expr.Literal(true, BOOL_TYPE);
        }

        if (match(NIL)) {
            return new Expr.Literal(null, NIL);
        }

        if (match(STR)) {
            return new Expr.Literal(previous().literal, STR_TYPE);
        }

        if (match(NUM)) {
            return new Expr.Literal(previous().literal, NUM_TYPE);
        }

        if (match(LEFT_PAREN)) {
            Expr expr = expression();
            consume(RIGHT_PAREN, "Expect ')' after expression.");
            return new Expr.Grouping(expr, expr.valueType);
        }

        if (match(IDENTIFIER)) {
            Token id = previous();
            TokenType id_type = variableTable.getType(id.lexeme);
            if (id_type == null) {
                throw error(previous(), "Undefined variable '" + previous().lexeme + "'.");
            }

            return new Expr.Variable(id, id_type);
        }

        if (match(FUN)) {
            return lambda();
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
            if (peek().tokenType == EOS) {
                advance();
                return;
            }

            switch (peek().tokenType) {
                case CLASS:
                case FUN:
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
            if (type != EOF) {
                throw error(peek(), "Unexpected end of file.");
            }
        }

        return peek().tokenType == type;
    }

    private Token advance() {
        if (isAtEnd()) {
            throw error(peek(), "Unexpected end of file.");
        }

        current++;
        return previous();
    }

    private boolean isAtEnd() {
        return peek().tokenType == EOF;
    }

    private Token peek() {
        return tokens.get(current);
    }

    private Token previous() {
        return tokens.get(current - 1);
    }
}
