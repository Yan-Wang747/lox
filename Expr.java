package lox;

import java.util.List;

public abstract class Expr {

    final Token valueType;

    Expr(Token valueType) {
        this.valueType = valueType;
    }

    interface Visitor<R> {
        R visit(List_ expr);
        R visit(Binary expr);
        R visit(Grouping expr);
        R visit(Literal expr);
        R visit(Variable expr);
        R visit(Unary expr);
        R visit(TernaryConditional expr);
    }

    abstract <R> R accept(Visitor<R> visitor);

    static class List_ extends Expr {

        final List<Expr> items;

        List_(List<Expr> items, Token valueType) {
            super(valueType);
            this.items = items;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Binary extends Expr {

        final Expr left;
        final Token operator;
        final Expr right;

        Binary(Expr left, Token operator, Expr right, Token valueType) {
            super(valueType);
            this.left = left;
            this.operator = operator;
            this.right = right;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Grouping extends Expr {

        final Expr expression;

        Grouping(Expr expression, Token valueType) {
            super(valueType);
            this.expression = expression;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Literal extends Expr {

        final Object value;

        Literal(Object value, Token valueType) {
            super(valueType);
            this.value = value;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Variable extends Expr {

        final Token name;
        final Expr indexExpr;

        Variable(Token name, Expr indexExpr, Token valueType) {
            super(valueType);
            this.name = name;
            this.indexExpr = indexExpr;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Unary extends Expr {

        final Token operator;
        final Expr right;

        Unary(Token operator, Expr right, Token valueType) {
            super(valueType);
            this.operator = operator;
            this.right = right;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class TernaryConditional extends Expr {

        final Expr condition;
        final Token question;
        final Expr thenBranch;
        final Expr elseBranch;

        TernaryConditional(Expr condition, Token question, Expr thenBranch, Expr elseBranch, Token valueType) {
            super(valueType);
            this.condition = condition;
            this.question = question;
            this.thenBranch = thenBranch;
            this.elseBranch = elseBranch;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }
}
