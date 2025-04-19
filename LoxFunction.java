package lox;

public class LoxFunction extends LoxLambda {
    private final Token funName;
    
    LoxFunction(Stmt.Function stmt, Environment closure) {
        super(stmt.lambda, closure);
        
        this.funName = stmt.name;
    }

    @Override
    public String toString() {
        return "<fn " + funName.lexeme + ">";
    }
}
