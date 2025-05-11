package lox;

import java.util.List;

public class LoxLambda implements LoxCallable{
    protected final Expr.Lambda lambdaExpr;
    protected final Environment closure;
    
    LoxLambda(Expr.Lambda lambdaExpr, Environment closure) {
        this.lambdaExpr = lambdaExpr;
        this.closure = closure;
    }

    @Override
    public int arity() {
        return lambdaExpr.params.size();
    }

    @Override
    public Object call(Interpreter interpreter, List<Object> arguments) {
        // Create a new environment for the function call
        Environment environment = new Environment(closure);
        for (int i = 0; i < lambdaExpr.params.size(); i++) {
            environment.define(lambdaExpr.params.get(i), arguments.get(i));
        }
        
        try{
            interpreter.executeBlock(lambdaExpr.body, environment);
        }
        catch (Return returnValue) {
            return returnValue.value;
        }
        
        return null; // If no return statement is executed
    }

    @Override
    public String toString() {
        return "<lambda>";
    }
}
