package lox;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;

public class LoxClass implements LoxCallable {
    final String name;
    private final Map<String, LoxFunction> methods;

    LoxClass(String name, Map<String, LoxFunction> methods) {
        this.name = name;
        this.methods = methods;
    }

    LoxFunction findMethod(String name) {
        return methods.get(name);
    }

    @Override
    public Object call(Interpreter interpreter, List<Object> arguments) {
        // Create a new instance of the class
        LoxInstance instance = new LoxInstance(this);
        return instance;
    }

    @Override
    public int arity() {
        // The class constructor
        return 0;
    }

    @Override
    public String toString() {
        return name;
    }
}
