package lox;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class Resolver implements Expr.Visitor<Void>, Stmt.Visitor<Void> {

    private enum FunctionType {
        NONE,
        FUNCTION,
        METHOD,
        INITIALIZER
    }

    private enum ClassType {
        NONE,
        CLASS,
        SUBCLASS
    }

    private enum LocalVarState {
        DECLARED,
        DEFINED,
        USED
    }
    
    private final Interpreter interpreter;
    private final Stack<Map<String, LocalVarState>> scopes = new Stack<>();
    private FunctionType currentFunction = FunctionType.NONE;
    private ClassType currentClass = ClassType.NONE;

    Resolver(Interpreter interpreter) {
        this.interpreter = interpreter;
    }

    @Override
    public Void visit(Stmt.Block stmt) {
        beginScope();
        resolve(stmt.statements);
        endScope();
        return null;
    }

    @Override
    public Void visit(Stmt.Class stmt) {
        ClassType enclosingClass = currentClass;
        currentClass = ClassType.CLASS;

        declare(stmt.name);
        define(stmt.name);

        beginScope();
        scopes.peek().put("this", LocalVarState.DEFINED);

        for (Stmt.Function method : stmt.methods) {
            FunctionType declaration = FunctionType.METHOD;
            if (method.name.lexeme.equals("init")) {
                declaration = FunctionType.INITIALIZER;
            }
            resolveFunction(method, declaration);
        }

        for (Stmt.Function method : stmt.staticMethods) {
            resolveFunction(method, FunctionType.METHOD);
        }

        endScope();

        currentClass = enclosingClass;
        return null;
    }

    void resolve(List<Stmt> statements) {
        for (Stmt statement : statements) {
            resolve(statement);
        }
    }

    private void resolve(Stmt stmt) {
        stmt.accept(this);
    }

    private void resolve(Expr expr) {
        expr.accept(this);
    }

    private void beginScope() {
        scopes.push(new HashMap<String, LocalVarState>());
    }

    private void endScope() {
        Map<String, LocalVarState> scope = scopes.pop();

        for (Map.Entry<String, LocalVarState> entry : scope.entrySet()) {
            if (entry.getKey().equals("this")) {
                continue;
            }
            if (entry.getValue() != LocalVarState.USED) {
                Lox.error("Variable '" + entry.getKey() + "' is never used.");
            }
        }
    }

    @Override
    public Void visit(Stmt.VarDecl stmt) {
        declare(stmt.name);
        if (stmt.initializer != null) {
            resolve(stmt.initializer);
        }
        define(stmt.name);
        return null;
    }

    private void declare(Token name) {
        if (scopes.isEmpty()) return;

        Map<String, LocalVarState> scope = scopes.peek();
        if (scope.containsKey(name.lexeme)) {
            Lox.error(name, "Variable with this name already declared in this scope.");
        }

        scope.put(name.lexeme, LocalVarState.DECLARED);
    }

    private void define(Token name) {
        if (scopes.isEmpty()) return;

        Map<String, LocalVarState> scope = scopes.peek();
        scope.put(name.lexeme, LocalVarState.DEFINED);
    }

    @Override
    public Void visit(Expr.Variable expr) {
        if (!scopes.isEmpty() &&
            scopes.peek().get(expr.name.lexeme) == LocalVarState.DECLARED) {
            Lox.error(expr.name, "Cannot read local variable in its own initializer.");
        }

        if (expr.name.tokenType == TokenType.THIS) {
            if (currentClass == ClassType.NONE) {
                Lox.error(expr.name, "Cannot use 'this' outside of a class.");
                return null;
            }
        }

        resolveLocal(expr, expr.name);
        return null;
    }

    private void resolveLocal(Expr.Variable expr, Token name) {
        for (int i = scopes.size() - 1; i >= 0; i--) {
            if (scopes.get(i).containsKey(name.lexeme)) {
                interpreter.resolve(expr, scopes.size() - 1 - i);
                scopes.get(i).put(name.lexeme, LocalVarState.USED);
                return;
            }
        }
    }

    @Override
    public Void visit(Stmt.Assign stmt) {
        resolve(stmt.value);

        if (!(stmt.target instanceof Expr.Variable)) {
            Lox.error(stmt.equal, "Invalid assignment target, only variables can be assigned to.");
            return null;
        }

        Expr.Variable target = (Expr.Variable) stmt.target;
        return visit(target);
    }

    @Override
    public Void visit(Stmt.Set stmt) {
        resolve(stmt.value);
        resolve(stmt.object);
        return null;
    }
    @Override
    public Void visit(Stmt.Function stmt) {
        declare(stmt.name);
        define(stmt.name);
        resolveFunction(stmt, FunctionType.FUNCTION);
        return null;
    }

    private void resolveFunction(Stmt.Function stmt, FunctionType type) {
        FunctionType enclosingFunction = currentFunction;
        currentFunction = type;

        beginScope();
        for (Token param : stmt.lambda.params) {
            declare(param);
            define(param);
        }
        resolve(stmt.lambda.body);
        endScope();

        currentFunction = enclosingFunction;
    }

    @Override
    public Void visit(Stmt.Expression stmt) {
        resolve(stmt.expression);
        return null;
    }

    @Override
    public Void visit(Stmt.If stmt) {
        resolve(stmt.condition);
        resolve(stmt.thenBranch);
        if (stmt.elseBranch != null) {
            resolve(stmt.elseBranch);
        }
        return null;
    }

    @Override
    public Void visit(Stmt.Print stmt) {
        resolve(stmt.expression);
        return null;
    }

    @Override
    public Void visit(Stmt.Return stmt) {
        if (currentFunction == FunctionType.NONE) {
            Lox.error(stmt.keyword, "Cannot return from top-level code.");
        }

        if (stmt.value != null) {
            if (currentFunction == FunctionType.INITIALIZER) {
                Lox.error(stmt.keyword, "Cannot return a value from an initializer.");
            }
            
            resolve(stmt.value);
        }

        return null;
    }

    @Override
    public Void visit(Stmt.While stmt) {
        resolve(stmt.condition);
        resolve(stmt.body);
        return null;
    }

    @Override
    public Void visit(Expr.Binary expr) {
        resolve(expr.left);
        resolve(expr.right);
        return null;
    }

    @Override
    public Void visit(Expr.Call expr) {
        resolve(expr.callee);
        for (Expr argument : expr.arguments) {
            resolve(argument);
        }
        return null;
    }

    @Override
    public Void visit(Expr.Get expr) {
        resolve(expr.object);
        return null;
    }

    @Override
    public Void visit(Expr.Grouping expr) {
        resolve(expr.expression);
        return null;
    }

    @Override
    public Void visit(Expr.Literal expr) {
        return null;
    }

    @Override
    public Void visit(Expr.Unary expr) {
        resolve(expr.right);
        return null;
    }

    @Override
    public Void visit(Expr.TernaryConditional expr) {
        resolve(expr.condition);
        resolve(expr.thenBranch);
        resolve(expr.elseBranch);
        return null;
    }

    @Override
    public Void visit(Expr.Lambda expr) {
        beginScope();
        for (Token param : expr.params) {
            declare(param);
            define(param);
        }
        resolve(expr.body);
        endScope();
        return null;
    }
}
