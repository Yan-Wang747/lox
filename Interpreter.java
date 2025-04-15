package lox;

import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

class Interpreter implements Expr.Visitor<Object>, Stmt.Visitor<Void> {
    
    Environment globals = new Environment();
    private Environment currentEnv = globals;

    Interpreter() {
        // Define the global variables here
        globals.define(new Token(TokenType.IDENTIFIER, "clock", null, 0), new LoxCallable() {
            @Override
            public int arity() {
                return 0;
            }

            @Override
            public Object call(Interpreter interpreter, List<Object> arguments) {
                return (double)System.currentTimeMillis() / 1000.0;
            }

            @Override
            public String toString() {
                return "<native fn>";
            }
        });
    }

    void interpret(List<Stmt> statements) {
        try {
            for (Stmt statement : statements) {
                execute(statement);
            }
        } catch (RuntimeError error) {
            Lox.runtimeError(error);
        }
    }

    private void execute(Stmt statement) {
        statement.accept(this);
    }

    private String stringify(Object object) {
        if (object == null) return "nil";

        if (object instanceof Double) {
            String text = object.toString();
            if (text.endsWith(".0")) {
                text = text.substring(0, text.length() - 2);
            }
            return text;
        }

        if (object instanceof ArrayList<?>) {
            StringBuilder sb = new StringBuilder("[");
            List<Object> list = (ArrayList<Object>) object;
            for (int i = 0; i < list.size(); i++) {
                sb.append(stringify(list.get(i)));
                if (i < list.size() - 1) {
                    sb.append(", ");
                }
            }
            sb.append("]");
            return sb.toString();
        }

        return object.toString();
    }

    private Object evaluate(Expr expr) {
        return expr.accept(this);
    }

    @Override
    public Void visit(Stmt.If stmt) {
        Object condition = evaluate(stmt.condition);
        if (!(condition instanceof Boolean))
            throw new RuntimeError(null, "Condition of if must be a boolean.");

        if ((boolean)condition) {
            execute(stmt.thenBranch);
        } else if (stmt.elseBranch != null) {
            execute(stmt.elseBranch);
        }
        return null;
    }

    @Override
    public Void visit(Stmt.Block stmt) {
        executeBlock(stmt.statements, new Environment(this.currentEnv));
        return null;
    }

    void executeBlock(List<Stmt> statements, Environment newEnvironment) {
        Environment enclosing = this.currentEnv;
        this.currentEnv = newEnvironment;
        try {
            for (Stmt statement : statements) {
                execute(statement);
            }
        } finally {
            this.currentEnv = enclosing;
        }
    }

    @Override
    public Void visit(Stmt.Assign stmt) {
        Object value = evaluate(stmt.value);
        currentEnv.assign(stmt.name, value);
        
        return null;
    }
    
    @Override
    public Void visit(Stmt.VarDecl stmt) {
        Object value = evaluate(stmt.initializer);

        currentEnv.define(stmt.name, value);
        return null;
    }

    @Override
    public Void visit(Stmt.Print stmt) {
        Object value = evaluate(stmt.expression);
        System.out.println(stringify(value));
        return null;
    }
    
    @Override
    public Void visit(Stmt.Return stmt) {
        Object value = null;
        if (stmt.value != null) {
            value = evaluate(stmt.value);
        }
        throw new Return(value);
    }
    
    @Override
    public Void visit(Stmt.While stmt) {
        // put the special variable break and contine to the env
        Token breakToken = new Token(TokenType.IDENTIFIER, "break", null, 0);
        Token continueToken = new Token(TokenType.IDENTIFIER, "continue", null, 0);
        currentEnv.define(breakToken, false);
        currentEnv.define(continueToken, false);
        
        while ((boolean)evaluate(stmt.condition)) {
            execute(stmt.body);
            if ((boolean)currentEnv.get(breakToken) && !(boolean)currentEnv.get(continueToken))
                break;

            if (stmt.increment != null) {
                execute(stmt.increment);
            }
            
        }
        return null;
    }
    
    @Override
    public Void visit(Stmt.Expression stmt) {
        evaluate(stmt.expression);
        return null;
    }

    @Override
    public Void visit(Stmt.Function stmt) {
        LoxFunction function = new LoxFunction(stmt);
        currentEnv.define(stmt.name, function);
        return null;
    }

    @Override
    public Object visit(Expr.List_ expr) {
        List<Object> values = new ArrayList<>();
        for (Expr item : expr.items) {
            values.add(evaluate(item));
        }
        return values;
    }

    @Override
    public Object visit(Expr.Literal expr) {
        return expr.value;
    }

    @Override
    public Object visit(Expr.Logical expr) {
        boolean left = (boolean) evaluate(expr.left);

        if (expr.operator.isTokenType(TokenType.OR)) {
            if (left) return left;
        } else {
            if (!left) return left;
        }

        return evaluate(expr.right);
    }

    @Override
    public Object visit(Expr.Grouping expr) {
        return evaluate(expr.expression);
    }

    @Override
    public Object visit(Expr.Unary expr) {
        Object right = evaluate(expr.right);

        switch (expr.operator.tokenType) {
            case MINUS:
                checkNumberOperand(expr.operator, right);
                return -(double) right;
            case BANG:
                if (right instanceof Boolean) return !(boolean)right;
                throw new RuntimeError(expr.operator, "Operand must be a boolean.");
            default:
                throw new RuntimeError(expr.operator, "Unknown unary operator.");
        }
    }

    @Override
    public Object visit(Expr.Call expr) {
        Object callee = evaluate(expr.callee);
        List<Object> arguments = new ArrayList<>();

        for (Expr argument : expr.arguments) {
            arguments.add(evaluate(argument));
        }

        if (!(callee instanceof LoxCallable)) {
            throw new RuntimeError(expr.paren, "Can only call functions and classes.");
        }

        LoxCallable function = (LoxCallable) callee;

        if (arguments.size() != function.arity()) {
            throw new RuntimeError(expr.paren, "Expected " + function.arity() + " arguments but got " + arguments.size() + ".");
        }

        return function.call(this, arguments);
    }

    @Override
    public Object visit(Expr.Variable expr) {
        return currentEnv.get(expr.name);
    }

    @Override
    public Object visit(Expr.Binary expr) {
        Object left = evaluate(expr.left);
        Object right = evaluate(expr.right);

        switch (expr.operator.tokenType) {
            case MINUS:
                checkNumberOperands(expr.operator, left, right);
                return (double)left - (double)right;
            case SLASH:
                checkNumberOperands(expr.operator, left, right);
                if ((double)right == 0) {
                    throw new RuntimeError(expr.operator, "Division by zero.");
                }
                
                return (double)left / (double)right;
            case STAR:
                checkNumberOperands(expr.operator, left, right);
                return (double)left * (double)right;
            case PLUS:
                if (left instanceof Double && right instanceof Double) {
                    return (double)left + (double)right;
                }
                if (left instanceof String && right instanceof String) {
                    return (String)left + (String)right;
                }
                if (left instanceof String || right instanceof String) {
                    return stringify(left) + stringify(right);
                }
                throw new RuntimeError(
                    expr.operator, 
                "Operands must be two numbers or strings."
                );
            case GREATER:
                try {
                    checkNumberOperands(expr.operator, left, right);
                    return (double)left > (double)right;
                }
                catch (RuntimeError e) {
                    checkStringOperands(expr.operator, left, right);
                    return ((String)left).compareTo((String)right) > 0;
                }
            case GREATER_EQUAL:
                checkNumberOperands(expr.operator, left, right);
                return (double)left >= (double)right;
            case LESS:
                checkNumberOperands(expr.operator, left, right);
                return (double)left < (double)right;
            case LESS_EQUAL:
                checkNumberOperands(expr.operator, left, right);
                return (double)left <= (double)right;
            case BANG_EQUAL:
                return !isEqual(left, right);
            case EQUAL_EQUAL:
                return isEqual(left, right);
            case COMMA:
                return right;
            default:
                throw new RuntimeError(expr.operator, "Unknown binary operator.");
        }
    }

    @Override
    public Object visit(Expr.TernaryConditional expr) {
        Object condition = evaluate(expr.condition);
        if (!(condition instanceof Boolean))
            throw new RuntimeError(expr.question, "Condition of ?: must be a boolean.");

        if ((Boolean)condition) {
            return evaluate(expr.thenBranch);
        } else {
            return evaluate(expr.elseBranch);
        }
    }

    private boolean isEqual(Object a, Object b) {
        if (a == null && b == null) return true;
        if (a == null) return false;

        return a.equals(b);
    }

    private void checkNumberOperand(Token operator, Object operand) {
        if (operand instanceof Double) return;
        throw new RuntimeError(operator, "Operand must be a number.");
    }

    private void checkNumberOperands(Token operator, Object left, Object right) {
        if (left instanceof Double && right instanceof Double) return;
        throw new RuntimeError(operator, "Operands must be numbers.");
    }

    private void checkStringOperands(Token operator, Object left, Object right) {
        if (left instanceof String && right instanceof String) return;
        throw new RuntimeError(operator, "Operands must be strings.");
    }
}
