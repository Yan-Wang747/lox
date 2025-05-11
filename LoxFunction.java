package lox;

public class LoxFunction extends LoxLambda {
    private final Token funName;
    
    LoxFunction(Stmt.Function stmt, Environment closure) {
        super(stmt.lambda, closure);
        
        this.funName = stmt.name;
    }

    LoxFunction bind(LoxInstance instance) {
        Environment environment = new Environment(closure);
        Token thisToken = new Token(TokenType.THIS, "this", null, 0);
        environment.define(thisToken, instance);
        Stmt.Function expr = new Stmt.Function(funName, lambdaExpr);
        return new LoxFunction(expr, environment);
    }

    @Override
    public String toString() {
        return "<fn " + funName.lexeme + ">";
    }
}
