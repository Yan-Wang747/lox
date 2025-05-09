package lox;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;

public class LoxClass implements LoxCallable {
    final String name;

    LoxClass(String name) {
        this.name = name;
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
