package lox;

import java.util.List;

abstract class Expr {

    final TokenType valueType;

    Expr(TokenType valueType) {
        this.valueType = valueType;
    }

    interface Visitor<R> {
        R visit(List_ expr);
        R visit(Binary expr);
        R visit(Grouping expr);
        R visit(Literal expr);
        R visit(Logical expr);
        R visit(Variable expr);
        R visit(Unary expr);
        R visit(TernaryConditional expr);
        R visit(Call expr);
    }

    abstract <R> R accept(Visitor<R> visitor);

    static class List_ extends Expr {

        final List<Expr> items;
        final TokenType valueType;

        List_(List<Expr> items, TokenType valueType) {
            super(valueType);
            this.items = items;
            this.valueType = valueType;
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
        final TokenType valueType;

        Binary(Expr left, Token operator, Expr right, TokenType valueType) {
            super(valueType);
            this.left = left;
            this.operator = operator;
            this.right = right;
            this.valueType = valueType;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Grouping extends Expr {

        final Expr expression;
        final TokenType valueType;

        Grouping(Expr expression, TokenType valueType) {
            super(valueType);
            this.expression = expression;
            this.valueType = valueType;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Literal extends Expr {

        final Object value;
        final TokenType valueType;

        Literal(Object value, TokenType valueType) {
            super(valueType);
            this.value = value;
            this.valueType = valueType;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Logical extends Expr {

        final Expr left;
        final Token operator;
        final Expr right;
        final TokenType valueType;

        Logical(Expr left, Token operator, Expr right, TokenType valueType) {
            super(valueType);
            this.left = left;
            this.operator = operator;
            this.right = right;
            this.valueType = valueType;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Variable extends Expr {

        final Token name;
        final TokenType valueType;

        Variable(Token name, TokenType valueType) {
            super(valueType);
            this.name = name;
            this.valueType = valueType;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }

    static class Unary extends Expr {

        final Token operator;
        final Expr right;
        final TokenType valueType;

        Unary(Token operator, Expr right, TokenType valueType) {
            super(valueType);
            this.operator = operator;
            this.right = right;
            this.valueType = valueType;
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
        final TokenType valueType;

        TernaryConditional(Expr condition, Token question, Expr thenBranch, Expr elseBranch, TokenType valueType) {
            super(valueType);
            this.condition = condition;
            this.question = question;
            this.thenBranch = thenBranch;
            this.elseBranch = elseBranch;
            this.valueType = valueType;
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
        final TokenType valueType;

        Call(Expr callee, Token paren, List<Expr> arguments, TokenType valueType) {
            super(valueType);
            this.callee = callee;
            this.paren = paren;
            this.arguments = arguments;
            this.valueType = valueType;
        }

        @Override
        <R> R accept(Visitor<R> visitor) {
            return visitor.visit(this);
        }
    }
}
