package lox;

import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

class Interpreter implements Expr.Visitor<Object>, Stmt.Visitor<Void> {
    
    Environment globals = new Environment();
    private Environment environment = globals;
    private final Map<Expr, Integer> locals = new HashMap<>();

    Interpreter() {
        // Define the global variables here
        globals.define("clock", new LoxCallable() {
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

    void resolve(Expr expr, int depth) {
        locals.put(expr, depth);
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
        executeBlock(stmt.statements, new Environment(this.environment));
        return null;
    }

    @Override
    public Void visit(Stmt.Class stmt) {
        Object superclass = null;
        if (stmt.superclass != null) {
            superclass = evaluate(stmt.superclass);
            if (!(superclass instanceof LoxClass)) {
                throw new RuntimeError(stmt.superclass.name, "Superclass must be a class.");
            }
        }

        environment.define(stmt.name.lexeme, null);

        if (stmt.superclass != null) {
            environment = new Environment(environment);
            environment.define("super", superclass);
        }

        Map<String, LoxFunction> methods = new HashMap<>();
        for (Stmt.Function method : stmt.methods) {
            boolean isInitializer = method.name.lexeme.equals("init");
            LoxFunction function = new LoxFunction(method, environment, isInitializer);
            methods.put(method.name.lexeme, function);
        }

        Map<String, LoxFunction> staticMethods = new HashMap<>();
        for (Stmt.Function method : stmt.staticMethods) {
            LoxFunction function = new LoxFunction(method, environment, false);
            staticMethods.put(method.name.lexeme, function);
        }

        LoxClass klass = new LoxClass(stmt.name.lexeme, (LoxClass)superclass, methods, staticMethods);
        
        if (superclass != null) {
            environment = environment.enclosing;
        }

        environment.assign(stmt.name, klass);

        return null;
    }

    void executeBlock(List<Stmt> statements, Environment newEnvironment) {
        Environment enclosing = this.environment;
        this.environment = newEnvironment;
        try {
            for (Stmt statement : statements) {
                execute(statement);
            }
        } finally {
            this.environment = enclosing;
        }
    }

    @Override
    public Void visit(Stmt.Assign stmt) {
        Expr.Variable target = (Expr.Variable) stmt.target;
        Object value = evaluate(stmt.value);

        Integer distance = locals.get(stmt.target);
        if (distance != null) {
            environment.assignAt(distance, target.name, value);
        } else {
            globals.assign(target.name, value);
        }
        
        return null;
    }

    @Override
    public Void visit(Stmt.Set stmt) {
        Object object = evaluate(stmt.object);
        if (object instanceof LoxInstance) {
            LoxInstance instance = (LoxInstance)object;
            Object value = evaluate(stmt.value);
            instance.set(stmt.name, value);
            return null;
        }
        else if (object instanceof LoxClass) {
            LoxClass klass = (LoxClass)object;
            Object value = evaluate(stmt.value);
            klass.set(stmt.name, value);
            return null;
        }
        throw new RuntimeError(stmt.name, "Only instances have fields.");
    }
    
    @Override
    public Void visit(Stmt.VarDecl stmt) {
        Object value = evaluate(stmt.initializer);

        environment.define(stmt.name.lexeme, value);
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
        environment.define("break", false);
        environment.define("continue", false);
        
        Object condition = evaluate(stmt.condition);
        if (!(condition instanceof Boolean))
            throw new RuntimeError(null, "Condition of while must be a boolean.");

        while ((boolean)condition) {
            execute(stmt.body);
            if ((boolean)environment.get(breakToken) && !(boolean)environment.get(continueToken))
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
        LoxFunction function = new LoxFunction(stmt, environment, false);
        environment.define(stmt.name.lexeme, function);
        return null;
    }

    @Override
    public LoxCallable visit(Expr.Lambda expr) {
        return new LoxLambda(expr, environment, false);
    }

    @Override
    public Object visit(Expr.Literal expr) {
        return expr.value;
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

        if (expr.callee instanceof Expr.Variable) {
            Token name = ((Expr.Variable)expr.callee).name;
            if (name.lexeme.equals("inner") && callee == null) {
                return null;
            }
        }

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
    public Object visit(Expr.Get expr) {
        Object object = evaluate(expr.object);
        if (object instanceof LoxInstance) {
            Object property = ((LoxInstance)object).get(expr.name);
            if (property instanceof LoxFunction && ((LoxFunction)property).lambdaExpr.isGetter) {
                return ((LoxFunction)property).call(this, new ArrayList<>());
            }
            else if (property instanceof LoxClass && ((LoxClass)property).findStaticMethod(expr.name.lexeme) != null) {
                return ((LoxClass)property).findStaticMethod(expr.name.lexeme);
            }
            return property;
        }
        else if (object instanceof LoxClass) {
            return ((LoxClass)object).get(expr.name);
        }
        else
            throw new RuntimeError(expr.name, expr.name.lexeme + " has no properties.");
    }
    
    @Override
    public Object visit(Expr.Variable expr) {
        return lookUpVariable(expr);
    }

    private Object lookUpVariable(Expr.Variable expr) {
        Token name = expr.name;
        Integer distance = locals.get(expr);
        if (distance != null) {
            return environment.getAt(distance, name.lexeme);
        } else {
            return globals.get(name);
        }
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
                else if (left instanceof String && right instanceof String) {
                    return (String)left + (String)right;
                }
                else if (left instanceof String || right instanceof String) {
                    return stringify(left) + stringify(right);
                }
                else {
                    throw new RuntimeError(expr.operator, "Operands must be two numbers or strings.");
                }
                
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
                try {
                    checkNumberOperands(expr.operator, left, right);
                    return (double)left >= (double)right;
                }
                catch (RuntimeError e) {
                    checkStringOperands(expr.operator, left, right);
                    return ((String)left).compareTo((String)right) >= 0;
                }
            case EQUAL_EQUAL:
                return isEqual(left, right);
            case COMMA:
                return right;
            case OR: case AND:
                boolean isLeftTrue = (boolean) evaluate(expr.left);

                if (expr.operator.tokenType == TokenType.OR) {
                    if (isLeftTrue) return left;
                } else {
                    if (!isLeftTrue) return left;
                }
        
                return evaluate(expr.right);
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
