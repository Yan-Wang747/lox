package lox.tool;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;


class GenerateAst {
    public static void main(String[] args) throws IOException {
        String outputDir = "lox";

        defineAst(outputDir, "Expr", Arrays.asList(
            "List_      : List<Expr> items, Token valueType",
            "Binary     : Expr left, Token operator, Expr right, Token valueType",
            "Grouping   : Expr expression, Token valueType",
            "Literal    : Object value, Token valueType",
            "Variable   : Token name, Expr indexExpr, Token valueType",
            "Unary      : Token operator, Expr right, Token valueType",
            "TernaryConditional : Expr condition, Token question, Expr thenBranch, Expr elseBranch, Token valueType"
            ));
        
        defineAst(outputDir, "Stmt", Arrays.asList(
            "If         : Expr condition, Stmt thenBranch, Stmt elseBranch",  
            "Block      : List<Stmt> statements",
            "Assign     : Token name, Expr index, Expr value",
            "Expression : Expr expression",
            "Print      : Expr expression",
            "VarDecl    : Token name, Expr initializer, boolean isMutable"
        ));

    }

    private static void defineAst(String outputDir, String baseName, List<String> types) throws IOException {
        String path = outputDir + "/" + baseName + ".java";
        PrintWriter writer = new PrintWriter(path, "UTF-8");

        writer.println("package lox;");
        writer.println();
        writer.println("import java.util.List;");
        writer.println();
        writer.println("public abstract class " + baseName + " {");
        writer.println();
        if (baseName.equals("Expr"))
            writer.println("    final Token valueType;");
        writer.println();

        // add the constructor
        if (baseName.equals("Expr")) {
            writer.println("    " + baseName + "(Token valueType) {");
            writer.println("        this.valueType = valueType;");
            writer.println("    }");
        }

        defineVisitor(writer, baseName, types);
        
        // The base accept() method
        writer.println();
        writer.println("    abstract <R> R accept(Visitor<R> visitor);");
        
        // The AST classes
        for (String type : types) {
            String className = type.split(":")[0].trim();
            String fields = type.split(":")[1].trim();
            defineSubClass(writer, baseName, className, fields);
        }

        writer.println("}");
        writer.close();
    }

    private static void defineSubClass(PrintWriter writer, String baseName, String className, String fieldList) {
        writer.println();
        writer.println("    static class " + className + " extends " + baseName + " {");
        writer.println();

        // Fields
        String[] fields = fieldList.split(", ");
        for (String field : fields) {
            if (field.equals("Token valueType")) continue;
            writer.println("        final " + field + ";");
        }
        writer.println();

        // Constructor
        writer.println("        " + className + "(" + fieldList + ") {");
        if (baseName.equals("Expr")) {
            writer.println("            super(valueType);");
        }

        // Store parameters in fields
        for (String field : fields) {
            if (field.equals("Token valueType")) continue;
            String name = field.split(" ")[1];
            writer.println("            this." + name + " = " + name + ";");
        }
        writer.println("        }"); // end of constructor
        
        // Visitor pattern
        writer.println();
        writer.println("        @Override");
        writer.println("        <R> R accept(Visitor<R> visitor) {");
        writer.println("            return visitor.visit(this);");
        writer.println("        }"); // end of accept method

        writer.println("    }"); // end of class
    }

    private static void defineVisitor(PrintWriter writer, String baseName, List<String> types) {
        writer.println();
        writer.println("    interface Visitor<R> {");

        for (String type : types) {
            String typeName = type.split(":")[0].trim();
            writer.println("        R visit(" + typeName + " " + baseName.toLowerCase() + ");");
        }

        writer.println("    }");
    }
}
