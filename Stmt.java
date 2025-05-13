package lox;

import java.util.List;

abstract class Stmt {

    interface Visitor<R> {
        R visit(If stmt);
        R visit(Block stmt);
        R visit(Class stmt);
        R visit(Assign stmt);
        R visit(Set stmt);
        R visit(Expression stmt);
        R visit(Function stmt);
        R visit(Print stmt);
        R visit(Return stmt);
        R visit(While stmt);
        R visit(VarDecl stmt);
    }

    abstract <R> R accept(Visitor<R> visitor);

    static class If extends Stmt {

        final Expr condition;
        final Stmt thenBranch;
        final Stmt elseBranch;

        If(Expr condition, Stmt thenBranch, Stmt elseBranch) {
            this.condition = condition;
            this.thenBranch = thenBranch;
            this.elseBranch = elseBranch;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Block extends Stmt {

        final List<Stmt> statements;

        Block(List<Stmt> statements) {
            this.statements = statements;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Class extends Stmt {

        final Token name;
        final List<Stmt.Function> methods;
        final List<Stmt.Function> staticMethods;

        Class(Token name, List<Stmt.Function> methods, List<Stmt.Function> staticMethods) {
            this.name = name;
            this.methods = methods;
            this.staticMethods = staticMethods;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Assign extends Stmt {

        final Expr target;
        final Token equal;
        final Expr value;

        Assign(Expr target, Token equal, Expr value) {
            this.target = target;
            this.equal = equal;
            this.value = value;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Set extends Stmt {

        final Expr object;
        final Token name;
        final Expr value;

        Set(Expr object, Token name, Expr value) {
            this.object = object;
            this.name = name;
            this.value = value;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Expression extends Stmt {

        final Expr expression;

        Expression(Expr expression) {
            this.expression = expression;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Function extends Stmt {

        final Token name;
        final Expr.Lambda lambda;

        Function(Token name, Expr.Lambda lambda) {
            this.name = name;
            this.lambda = lambda;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Print extends Stmt {

        final Expr expression;

        Print(Expr expression) {
            this.expression = expression;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Return extends Stmt {

        final Token keyword;
        final Expr value;

        Return(Token keyword, Expr value) {
            this.keyword = keyword;
            this.value = value;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class While extends Stmt {

        final Expr condition;
        final Stmt body;
        final Stmt increment;

        While(Expr condition, Stmt body, Stmt increment) {
            this.condition = condition;
            this.body = body;
            this.increment = increment;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class VarDecl extends Stmt {

        final Token mut;
        final Token name;
        final Expr initializer;

        VarDecl(Token mut, Token name, Expr initializer) {
            this.mut = mut;
            this.name = name;
            this.initializer = initializer;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }
}
