package lox;

import java.util.List;

public class LoxLambda implements LoxCallable{
    protected final Expr.Lambda lambdaExpr;
    protected final Environment closure;
    protected final boolean isInitializer;
    
    LoxLambda(Expr.Lambda lambdaExpr, Environment closure, boolean isInitializer) {
        this.lambdaExpr = lambdaExpr;
        this.closure = closure;
        this.isInitializer = isInitializer;
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
            if (isInitializer) { // if the function is an initializer and a return at the end
                return closure.getAt(0, "this");
            }
            return returnValue.value;
        }
        
        if (isInitializer) { // if the function is an initializer and no return
            return closure.getAt(0, "this");
        }
        return null; // If no return statement is executed
    }

    @Override
    public String toString() {
        return "<lambda>";
    }
}
