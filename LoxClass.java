package lox;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class LoxClass implements LoxCallable {
    final String name;
    final LoxClass superclass;
    private final Map<String, LoxFunction> methods;
    private final Map<String, LoxFunction> staticMethods;
    private final Map<String, Object> staticFields = new HashMap<>();

    LoxClass(
        String name,
        LoxClass superclass,
        Map<String, LoxFunction> methods, Map<String, LoxFunction> staticMethods) 
    {
        this.superclass = superclass;
        this.name = name;
        this.methods = methods;
        this.staticMethods = staticMethods;
    }

    LoxFunction findMethod(String name) {
        if (methods.containsKey(name)) {
            return methods.get(name);
        }

        if (superclass != null) {
            return superclass.findMethod(name);
        }
        return null;
    }

    LoxFunction findStaticMethod(String name) {
        return staticMethods.get(name);
    }

    @Override
    public Object call(Interpreter interpreter, List<Object> arguments) {
        // Create a new instance of the class
        LoxInstance instance = new LoxInstance(this);
        LoxFunction initializer = findMethod("init");
        if (initializer != null) {
            initializer.bind(instance).call(interpreter, arguments);
        }

        return instance;
    }

    @Override
    public int arity() {
        LoxFunction initializer = findMethod("init");
        if (initializer == null) return 0;
        return initializer.arity();
    }

    Object get(Token name) {
        // handle static properties
        if (staticFields.containsKey(name.lexeme)) {
            return staticFields.get(name.lexeme);
        }

        LoxFunction method = findStaticMethod(name.lexeme);
        if (method != null) {
            return method;
        }

        throw new RuntimeError(name, "Undefined static property '" + name.lexeme + "'.");
    }

    void set(Token name, Object value) {
        // handle static properties
        staticFields.put(name.lexeme, value);
        return;
    }
    
    @Override
    public String toString() {
        return name;
    }
}
