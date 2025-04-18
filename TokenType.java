package lox;

enum TokenType {
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SLASH, STAR,

    BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,

    IDENTIFIER, STR, NUM,

    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, MUT, WHILE,
    BREAK, CONTINUE,

    QUESTION, COLON, SEMICOLON,

    STR_TYPE, NUM_TYPE, BOOL_TYPE, CALLABLE, // type keywords

    EOF
}
