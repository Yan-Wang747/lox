package lox.tool;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;


class GenerateAst {
    public static void main(String[] args) throws IOException {
        String outputDir = "lox";

        defineAst(outputDir, "Expr", Arrays.asList(
            "Binary     : Expr left, Token operator, Expr right",
            "Grouping   : Expr expression",
            "Literal    : Object value",
            "Variable   : Token name",
            "Unary      : Token operator, Expr right",
            "TernaryConditional : Expr condition, Token question, Expr thenBranch, Expr elseBranch",
            "Call       : Expr callee, Token paren, List<Expr> arguments",
            "Get        : Expr object, Token name",
            "Super      : Token keyword, Token method",
            "Lambda     : List<Token> params, List<Stmt> body, boolean isGetter"
            ));
        
        defineAst(outputDir, "Stmt", Arrays.asList(
            "If         : Expr condition, Stmt thenBranch, Stmt elseBranch",  
            "Block      : List<Stmt> statements",
            "Class      : Token name, Expr.Variable superclass, List<Stmt.Function> methods, List<Stmt.Function> staticMethods",
            "Assign     : Expr target, Token equal, Expr value",
            "Set        : Expr object, Token name, Expr value",
            "Expression : Expr expression",
            "Function   : Token name, Expr.Lambda lambda",
            "Print      : Expr expression",
            "Return     : Token keyword, Expr value",
            "While      : Expr condition, Stmt body, Stmt increment",
            "VarDecl    : Token mut, Token name, Expr initializer"
        ));

    }

    private static void defineAst(String outputDir, String baseName, List<String> types) throws IOException {
        String path = outputDir + "/" + baseName + ".java";
        PrintWriter writer = new PrintWriter(path, "UTF-8");

        writer.println("package lox;");
        writer.println();
        writer.println("import java.util.List;");
        writer.println();
        writer.println("abstract class " + baseName + " {");
        writer.println();

        defineVisitor(writer, baseName, types);
        
        // The base accept() method
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
            writer.println("        final " + field + ";");
        }
        writer.println();

        // Constructor
        writer.println("        " + className + "(" + fieldList + ") {");

        // Store parameters in fields
        for (String field : fields) {
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
        writer.println("    interface Visitor<R> {");

        for (String type : types) {
            String typeName = type.split(":")[0].trim();
            writer.println("        R visit(" + typeName + " " + baseName.toLowerCase() + ");");
        }

        writer.println("    }");
        writer.println();
    }
}
