package lox;

class Token {
    final TokenType tokenType;
    final String lexeme;
    final Object literal;
    final int line;

    Token(TokenType tokenType, String lexeme, Object literal, int line) {
        this.tokenType = tokenType;
        this.lexeme = lexeme;
        this.literal = literal;
        this.line = line;
    }

    public String toString() {
        return tokenType + " " + lexeme + " " + literal;
    }

    public boolean isTokenType(TokenType type) {
        return this.tokenType == type;
    }

    public boolean isNotTokenType(TokenType type) {
        return !this.isTokenType(type);
    }
}
