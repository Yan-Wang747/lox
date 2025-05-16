package lox;

public class LoxFunction extends LoxLambda {
    private final Token funName;
    
    LoxFunction(Stmt.Function stmt, Environment closure, boolean isInitializer) {
        super(stmt.lambda, closure, isInitializer);
        
        this.funName = stmt.name;
    }

    LoxFunction bind(LoxInstance instance) {
        Environment environment = new Environment(closure);
        environment.define("this", instance);
        environment.define("inner", instance.klass.findSelfMethod(funName.lexeme));
        Stmt.Function expr = new Stmt.Function(funName, lambdaExpr);
        return new LoxFunction(expr, environment, isInitializer);
    }

    @Override
    public String toString() {
        return "<fn " + funName.lexeme + ">";
    }
}
