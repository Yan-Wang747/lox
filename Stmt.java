package lox;

import java.util.List;

abstract class Stmt {

    interface Visitor<R> {
        R visit(If stmt);
        R visit(Block stmt);
        R visit(Assign stmt);
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

    static class Assign extends Stmt {

        final Token name;
        final Expr value;

        Assign(Token name, Expr value) {
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
        final List<Token> params;
        final List<Stmt> body;

        Function(Token name, List<Token> params, List<Stmt> body) {
            this.name = name;
            this.params = params;
            this.body = body;
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

        final Token name;
        final Expr initializer;
        final boolean isMutable;

        VarDecl(Token name, Expr initializer, boolean isMutable) {
            this.name = name;
            this.initializer = initializer;
            this.isMutable = isMutable;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }
}
