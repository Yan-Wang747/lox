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

    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!(obj instanceof Token)) return false;
        Token other = (Token) obj;
        if (this.literal != null)
            return tokenType == other.tokenType && lexeme.equals(other.lexeme) && literal.equals(other.literal);
        else{
            return tokenType == other.tokenType && lexeme.equals(other.lexeme) && other.literal == null;
        }
    }
}
