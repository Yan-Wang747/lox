package lox;

import java.util.List;

abstract class Expr {

    interface Visitor<R> {
        R visit(Binary expr);
        R visit(Grouping expr);
        R visit(Literal expr);
        R visit(Variable expr);
        R visit(Unary expr);
        R visit(TernaryConditional expr);
        R visit(Call expr);
        R visit(Get expr);
        R visit(Lambda expr);
    }

    abstract <R> R accept(Visitor<R> visitor);

    static class Binary extends Expr {

        final Expr left;
        final Token operator;
        final Expr right;

        Binary(Expr left, Token operator, Expr right) {
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

        Grouping(Expr expression) {
            this.expression = expression;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Literal extends Expr {

        final Object value;

        Literal(Object value) {
            this.value = value;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Variable extends Expr {

        final Token name;

        Variable(Token name) {
            this.name = name;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Unary extends Expr {

        final Token operator;
        final Expr right;

        Unary(Token operator, Expr right) {
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

        TernaryConditional(Expr condition, Token question, Expr thenBranch, Expr elseBranch) {
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

    static class Call extends Expr {

        final Expr callee;
        final Token paren;
        final List<Expr> arguments;

        Call(Expr callee, Token paren, List<Expr> arguments) {
            this.callee = callee;
            this.paren = paren;
            this.arguments = arguments;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Get extends Expr {

        final Expr object;
        final Token name;

        Get(Expr object, Token name) {
            this.object = object;
            this.name = name;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Lambda extends Expr {

        final List<Token> params;
        final List<Stmt> body;
        final boolean isGetter;

        Lambda(List<Token> params, List<Stmt> body, boolean isGetter) {
            this.params = params;
            this.body = body;
            this.isGetter = isGetter;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }
}
