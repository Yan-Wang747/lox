package lox;

import java.util.List;

public abstract class Stmt {



    interface Visitor<R> {
        R visit(If stmt);
        R visit(Block stmt);
        R visit(Assign stmt);
        R visit(Expression stmt);
        R visit(Print stmt);
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
        final Expr index;
        final Expr value;

        Assign(Token name, Expr index, Expr value) {
            this.name = name;
            this.index = index;
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
