package lox;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static lox.TokenType.*;

class Scanner {
    private final String source;
    private final List<Token> tokens = new ArrayList<>();
    private int start = 0;
    private int current = 0;
    private int line = 1;
    private static final Map<String, TokenType> keywords;
    static {
        keywords = new HashMap<>();
        keywords.put("and",    AND);
        keywords.put("class",  CLASS);
        keywords.put("else",   ELSE);
        keywords.put("false",  FALSE);
        keywords.put("for",    FOR);
        keywords.put("fun",    FUN);
        keywords.put("if",     IF);
        keywords.put("nil",    NIL);
        keywords.put("or",     OR);
        keywords.put("print",  PRINT);
        keywords.put("return", RETURN);
        keywords.put("super",  SUPER);
        keywords.put("this",   THIS);
        keywords.put("true",   TRUE);
        keywords.put("mut",    MUT);
        keywords.put("var",    VAR);
        keywords.put("while",  WHILE);
        keywords.put("break",  BREAK);
        keywords.put("continue", CONTINUE);
        keywords.put("str",    STR_TYPE);
        keywords.put("num",    NUM_TYPE);
        keywords.put("bool",   BOOL_TYPE);
    }

    Scanner(String source) {
        this.source = source;

        // remove leading white spaces to avoid empty statements
        discard_whitespaces();
    }

    List<Token> scanTokens() {
        while (!isAtEnd()) {
            start = current;
            scanToken();
        }

        // add an NL, EOF token at the end
        if (tokens.get(tokens.size() - 1).isNotTokenType(EOS))
            tokens.add(new Token(EOS, "", null, line));
        
        tokens.add(new Token(EOF, "", null, line));
        return tokens;
    }

    private boolean isAtEnd() {
        return current >= source.length();
    }

    private void scanToken() {
        char c = advance();

        // shortcut for digits
        if (isDigit(c)) {
            number();
            return;
        }

        // keywords and identifiers
        if (isAlpha(c)) {
            identifier();
            return;
        }

        switch (c) {
            case '?': addToken(QUESTION); break;
            case ':': addToken(COLON); break;
            case ';': 
                addToken(EOS); 
                discard_whitespaces();
                break;
            case '(': addToken(LEFT_PAREN); break;
            case ')': addToken(RIGHT_PAREN); break;
            case '{': addToken(LEFT_BRACE); break;
            case '}': addToken(RIGHT_BRACE); break;
            case ',': addToken(COMMA); break;
            case '.': addToken(DOT); break;
            case '-': addToken(MINUS); break;
            case '+': addToken(PLUS); break;
            case '*': addToken(STAR); break;
            case '!': addToken(match('=') ? BANG_EQUAL : BANG); break;
            case '=': addToken(match('=') ? EQUAL_EQUAL : EQUAL); break;
            case '<': addToken(match('=') ? LESS_EQUAL : LESS); break;
            case '>': addToken(match('=') ? GREATER_EQUAL : GREATER); break;
            case '/':
                if (match('/'))
                    while (peek() != '\n' && !isAtEnd()) 
                        advance();
                
                else if (match('*')) { // handle /* */ comments
                    while ((peek() != '*' || peekNext() != '/') && !isAtEnd()) {
                        if (peek() == '\n') line++;
                        advance();
                    }
                    if (isAtEnd()) {
                        Lox.error(line, "Unterminated block comment.");
                        return;
                    }
                    advance(); // consume the '*'
                    advance(); // consume the '/'
                }
                    
                else
                    addToken(SLASH);
                break;
            case '\\': 
                // ignore white spaces following backslashes
                discard_whitespaces(); break; 
            case ' ':
            case '\r':
            case '\t':
                break;
            case '\n': // also used as end of statement
                // skip duplicate new lines, speed up a little
                addToken(EOS);
                line++;
                discard_whitespaces();
                break;
            case '"': 
                string(); 
                break;
            default:
                Lox.error(line, "Unexpected character '" + c + "'.");
                break;
        }
    }

    private boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    private boolean isAlpha(char c) {
        return (c >= 'a' && c <= 'z') ||
               (c >= 'A' && c <= 'Z') ||
               c == '_';
    }

    private boolean isAlphaNumeric(char c) {
        return isAlpha(c) || isDigit(c);
    }

    private void identifier() {
        while (isAlphaNumeric(peek())) advance();

        String text = source.substring(start, current);
        TokenType type = keywords.get(text);
        if (type == null) type = IDENTIFIER;
        addToken(type);
    }
    
    private void number() {
        while (isDigit(peek())) advance();

        // look for fractional part
        if (peek() == '.' && isDigit(peekNext())) {
            advance(); // consume the '.'
            while (isDigit(peek())) advance();
        }

        addToken(NUM, Double.parseDouble(source.substring(start, current)));
    }
    
    private void string() {
        while (peek() != '"' && !isAtEnd() && peek() != '\n')
            advance();

        if (isAtEnd() || peek() == '\n') {
            Lox.error(line, "Unterminated string.");
            return;
        }

        advance(); // closing quote
        
        // trim the surrounding quotes
        String value = source.substring(start + 1, current - 1);
        addToken(STR, value);
    }

    private boolean match(char expected) {
        if (isAtEnd()) return false;
        if (source.charAt(current) != expected) return false;

        current++;
        return true;
    }

    private char peek() {
        if (isAtEnd()) return '\0';
        return source.charAt(current);
    }

    private char peekNext() {
        if (current + 1 >= source.length()) return '\0';
        return source.charAt(current + 1);
    }

    private char advance() {
        return source.charAt(current++);
    }

    private void discard_whitespaces(){ 
        while (!isAtEnd() && Character.isWhitespace(peek())) {
            char c = advance();
            if (c == '\n')
                line++;
        }
    }

    private void addToken(TokenType type) {
        addToken(type, null);
    }

    private void addToken(TokenType type, Object literal) {
        String text = source.substring(start, current);
        tokens.add(new Token(type, text, literal, line));
    }
}
